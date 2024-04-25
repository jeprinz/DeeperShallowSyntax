open Util

type id = Id of int
  [@@deriving show]
let nextId : int ref = ref 0
let freshId (_ : unit) : id =
  let next = !nextId in
  nextId := !nextId + 1
  ; Id next

type label = 
  MetaVar of id
  | Lam (*1 child*)
  | App (*2 childred*)
  | Var of int (*0 children*)
  | Const of string (*0 children*)
  (*| StringLiteral of string (*0 children*)*)
  | MetaData of string (*1 child*)

type term = label tree

let freshMetaVar (_ : unit) : term = (* TODO: use this later on*)
  node (MetaVar (freshId ())) []

let rec show_term_impl (lamParens : bool) (appParens : bool) (t : term) : string =
  match t with
  | Node(Lam, [body]) -> let inside = "λ " ^ show_term_impl false false body in
    if lamParens then "(" ^ inside ^ ")" else inside
  | Node(App, [t1; t2]) ->
      let inside = show_term_impl true false t1 ^ " " ^ show_term_impl true true t2 in
      if appParens then "(" ^ inside ^ ")" else inside
  | Node(Var n, []) -> string_of_int n
  | Node(Const s, []) -> s
  (* | Node(StringLiteral s, []) -> s *)
  | Node(MetaData d, [t]) -> "MD[" ^ d ^ "][" ^ show_term_impl false false t ^ "]"
  | Node(MetaVar x, []) -> "M" ^ show_id x
  | _ -> raise (Error "show_term")

let show_term = show_term_impl false false

module IntMap = Map.Make (Int)
type sub = term IntMap.t 
type subList = (int * term) list (*[@@deriving show]*)

let show_sub (s : sub) : string =
  "{" ^ IntMap.fold (fun key t acc -> string_of_int key ^ " -> " ^ show_term t ^ ", " ^ acc) s "" ^ "}"

(* Assumes that t is closed *)
let rec metaSubst (menv : sub) (t : term) : term =
  match t with
  | Node(MetaVar (Id x), []) -> (match IntMap.find_opt x menv with
    | Some t -> metaSubst menv t
    | None -> t)
  | Node(l, kids) -> Node(l, List.map (metaSubst menv) kids)

module IntSet = Set.Make (Int)

let freeVars (t : term) : IntSet.t =
  let rec impl (t : term) (ctxLength : int) (acc : IntSet.t) =
    match t with
    | Node(Lam, [body]) -> impl body (ctxLength + 1) acc
    | Node(Var n, []) -> if n < ctxLength then acc else IntSet.add (n - ctxLength) acc
    | Node(_l, kids) -> List.fold_right (fun t acc -> impl t ctxLength acc) kids acc
  in
  impl t 0 IntSet.empty

let rec lift (k : int) (t : term) : term =
  match t with
  | Node(Var i, []) -> if i < k then Node(Var i, []) else Node(Var (i + 1), [])
  | Node(Lam,  [t]) -> Node(Lam, [lift (k + 1) t])
  | Node(l, kids) -> Node(l, List.map (lift k) kids)

let rec subst (index : int) (t' : term) (t : term) : term =
  match t with
  | Node(Var i, []) -> if index < i then Node(Var (i - 1), []) else if i = index then t' else Node(Var i, [])
  | Node(Lam,  [body]) -> Node(Lam, [subst (index + 1) (lift 0 t') body])
  | Node(l, kids) -> Node(l, List.map (subst index t') kids)

(*This implementation is not fast*)
let rec norm (t : term) : term =
  match t with
  | Node(Lam, [Node(App, [t; Node(Var 0, [])])]) -> norm t (*function eta*)
  | Node(MetaData _,  [t]) -> t
  | Node(App, [Node(Lam, [t1]); t2]) -> norm (subst 0 t2 t1)
  | Node(App, [t1; t2]) ->
      let t1' = norm t1 in
      let t2' = norm t2 in
      let res = Node(App, [t1'; t2']) in
      if t1' = t1 && t2 = t2' then res else norm res
  | Node(l, kids) -> Node(l, List.map norm kids)

type equation = term * term
exception Failure of equation
type ('s, 'a) state = 's ref -> 'a
type 't unifyM = (sub, 't) state

(* Assumes that the input is normalized *)
(* The list goes from innermost to outermost. E.g,  (x a b c) -> (x, [a :: b :: c :: []])*)
let isTelescope (t : term) : (label * (term list)) option =
  let rec impl (t : term) (acc : term list) =
    match t with
    | Node((MetaVar _) as l, []) -> Some (l, acc)
    | Node((Var _) as l, []) -> Some (l, acc)
    | Node((Const _) as l, []) -> Some (l, acc)
    | Node(App, [t1; t2]) -> Option.bind (impl t1 (t2 :: acc)) (fun (l, ts) -> Some (l, ts))
    | _ -> None
  in impl t []

let rec fromTelescope (t : term) (args : term list) : term =
  match args with
  | [] -> t
  | arg :: args -> fromTelescope (Node(App, [t; arg])) args

let rec telescopeEquation (ifFail : exn) (tel1 : term list) (tel2 : term list) : equation list =
  match tel1, tel2 with
  | [], [] -> []
  | arg1 :: args1, arg2 :: args2 -> (arg1, arg2) :: (telescopeEquation ifFail args1 args2)
  | _ -> raise ifFail

(* Checks if l1 is a prefix of l2, and returns the rest of l2 after the prefix *)
let rec isPrefix (l1 : 't list) (l2 : 't list) : 't list option =
  match l1, l2 with
  | [], _ -> Some l2
  | x :: xs, y :: ys -> if x = y then isPrefix xs ys else None
  | _ -> None

(*
Given u_0 ... u_{n-1}, and v in context gamma,
find v' in a context (gamma + n) such that
v'[0/u0]...[n-1/u_{n-1}] = v

Returns None if such a v' doesn't uniquely exist.

neutralForms is a mapping of variables to telescopes that they are applied to
in the order they appear in context.
*)
exception WasNotOfForm
let reverseSubstituteNeutralForms
  (neutralForms : (int * term list) list)
  (normalForm : term) : term option =
  (* Throws WasNotOfForm if the term couldn't be reversed substituted *)
  let rec impl (boundVars : int) (normalForm : term) : term =
    match normalForm with
    | Node(Lam, [body]) -> (Node(Lam, [impl (boundVars + 1) body]))
    | t -> match isTelescope t with
      | Some (Var i, args) when i >= boundVars -> (
        let trueIndex = i - boundVars in
        (* Find a neutral form from the list which is a prefix of this one *)
        let postFix = find_map_index (fun neutralIndex (i, neutralArgs) ->
            if i <> trueIndex then None else
            Option.bind (isPrefix neutralArgs args) (fun postFix -> Some (neutralIndex, postFix)))
            neutralForms in
        match postFix with
        | None -> raise WasNotOfForm
        | Some (index, postFix) -> fromTelescope (Node (Var index, [])) (List.map (impl boundVars) postFix)
      )
      | Some (l, args) -> fromTelescope (Node (l, [])) (List.map (impl boundVars) args)
      | None -> raise (Error "Term wasn't Lambda or telescope")
  in
    try
      (* All free variables in normalForm should either get substituted, or cause an error *)
      Some (impl 0 normalForm)
    with WasNotOfForm -> None

(* checks if each term is of the form (x ...) and none is a prefix of another.
   If they are of the correct form, returns the telescopes. *)
let termsAreUniqueNeutrals (ts : term list) : ((int * term list) list) option =
    Option.bind (sequence (List.map (fun t -> Option.bind (isTelescope t) (fun (l, args) ->
        match l with
        | (Var i) -> Some (i, args)
        | _ -> None))
      ts)) (fun telescopes ->
        (* each t is of the form (x ...), now we have to check if they are distinct *)
        (* This checks every pair *)
        if allPairs (fun (x, argsx) (y, argsy) -> not (x = y && (Option.is_some (isPrefix argsx argsy) || Option.is_some (isPrefix argsy argsx)))
          ) telescopes
          then Some telescopes else None
        )

let processEq : (equation -> (equation list) option) unifyM =
  fun env (t1Pre, t2Pre) ->
  let (t1, t2) as eq = (norm (metaSubst !env t1Pre), norm (metaSubst !env t2Pre)) in
  let ifFail = Failure eq in
  match t1, t2, isTelescope t1, isTelescope t2 with
  | Node(Lam, [t1]), Node(Lam, [t2]), _, _ -> Some [(t1, t2)]
  (* One of the terms is a metavar telescope *)
  | _, t, Some (MetaVar (Id mv), args), _
  | t, _, _, Some (MetaVar (Id mv), args)-> (
    (* reverse substitute neutral forms *)
    (*  *)
    Option.bind (termsAreUniqueNeutrals args) (fun telescopes ->
      (* We need to reverse the order of the neutral forms because that is the order that the variables will be in context *)
      Option.bind (reverseSubstituteNeutralForms (List.rev telescopes) t) (fun subbedValue ->
        (* Wrap the reverse subbed value in a bunch lambdas, one for each argument in the telescope *)
        let value = List.fold_right (fun _ t -> Node(Lam, [t])) telescopes subbedValue in
        env := IntMap.add mv value !env; Some [])
      )
    )
  (* Both of the terms are neutral forms *)
  | _, _, Some (Var _ as l1, argsx), Some (Var _ as l2, argsy)
  | _, _, Some (Const _ as l1, argsx), Some (Const _ as l2, argsy) ->
    if l1 <> l2 then raise ifFail else Some (telescopeEquation ifFail argsx argsy)
  | _ -> raise ifFail

(* Note that the resulting substitution is NOT idempotent *)
let rec unifyImpl : (bool -> ('metadata * equation) list -> ('metadata * equation) list -> ('metadata * equation) list) unifyM =
  fun env madeProgress eqs todo -> match eqs with
    | (md, e) :: es -> (
        let newEqs = processEq env e in
        match newEqs with
        | None -> unifyImpl env madeProgress es ((md, e) :: todo)
        | Some eqs' -> unifyImpl env true (List.map (fun eq -> (md, eq)) eqs' @ es) todo
        )
    | [] -> if madeProgress then unifyImpl env false todo [] else todo

let unify (eqs:equation list) : (sub * equation list) option =
  try
    let sub = ref IntMap.empty in
    let finalEqs = unifyImpl sub false (List.map (fun e -> ((), e)) eqs) [] in
    Some (!sub, List.map snd finalEqs)
  with Failure(_t1, _t2) ->
    (* print_endline ("unify failed with: " ^ show_term t1 ^ " != " ^ show_term t2 ); *)
    None

let unifyPartially (env : sub) (eqs : ('metadata * equation) list) : (sub * ('metadata * equation) list) option =
  try
    let sub = ref env in
    let finalEqs = unifyImpl sub false eqs [] in
    Some (!sub, finalEqs)
  with Failure(_) -> None