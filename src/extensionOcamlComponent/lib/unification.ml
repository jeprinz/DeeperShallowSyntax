open Stdlib

(* Higher order unification! *)

type ('s, 'a) state = 's ref -> 'a

type id = int
  [@@deriving show]
let nextId : int ref = ref 0
let freshId (_ : unit) : id =
  let next = !nextId in
  nextId := !nextId + 1
  ; next

type term = MetaVar of id | Lam of term | App of term * term | Var of int
  | Pair of term * term | Proj1 | Proj2
  | Const of string | MetaData of string * term
  (* [@@deriving show] *)

type valueTag = TLam | TVar | TPair | TProj1 | TProj2 | TConst

let getValueTag (t : term) : valueTag option =
  match t with
  | Lam _ -> Some TLam
  | Var _ -> Some TVar
  | Pair(_, _) -> Some TPair
  | Proj1 -> Some TProj1
  | Proj2 -> Some TProj2
  | Const _ -> Some TConst
  | _ -> None

let rec show_term_impl (lamParens : bool) (appParens : bool) (t : term) : string =
  match t with
  | Lam body -> let inside = "λ " ^ show_term_impl false false body in
    if lamParens then "(" ^ inside ^ ")" else inside
  | App (t1, t2) ->
      let inside = show_term_impl true false t1 ^ " " ^ show_term_impl false true t2 in
      if appParens then "(" ^ inside ^ ")" else inside
  | Pair (t1, t2) -> "(" ^ show_term_impl false false t1 ^ "," ^ show_term_impl false false t2 ^ ")"
  | Var n -> show_id n
  | Proj1 -> "proj1"
  | Proj2 -> "proj2"
  | Const s -> s
  | MetaData (d, t) -> "MD[" ^ d ^ "][" ^ show_term_impl false false t ^ "]"
  | MetaVar x -> "M" ^ show_id x

let show_term = show_term_impl false false
(* let pp_term : Formatter.formatter -> term -> unit *)


let children (t : term) : term list =
  match t with
  | Lam t -> [t]
  | App (t1, t2) -> [t1; t2]
  | Pair (t1, t2) -> [t1; t2]
  | MetaData (_, t) -> [t]
  | _ -> []

module IntMap = Map.Make (Int)
type sub = term IntMap.t 
type subList = (int * term) list (*[@@deriving show]*)

let rec metaSubst (menv : sub) (t : term) : term =
  let recur = metaSubst menv in
  match t with
  | Lam body -> Lam (recur body)
  | App (t1, t2) -> App (recur t1, recur t2)
  | Pair (t1, t2) -> Pair (recur t1, recur t2)
  | Var _ | Proj1 | Proj2 | Const _ -> t 
  | MetaData (d, t) -> MetaData(d, recur t)
  | MetaVar x -> (match IntMap.find_opt x menv with
    | Some t -> recur t
    | None -> t)

type 't unifyM = (sub, 't) state

let rec not_occurs (env : sub) (x : id) (t : term) : bool =
    match t with
    | MetaVar y -> (match IntMap.find_opt y env with
      | Some t' -> not_occurs env x t'
      | None -> not (x = y))
    | _ -> List.for_all (not_occurs env x) (children t)

let rec var_not_occurs (env : sub) (x : id) (t : term) : bool =
    match t with
    | Var y -> not (x = y)
    | MetaVar y -> (match IntMap.find_opt y env with
      | Some t' -> var_not_occurs env x t'
      | None -> true)
    | _ -> List.for_all (var_not_occurs env x) (children t)

type equation = term * term
(* type equations = equation list [@@deriving show] *)
let show_list (l : string list) : string =
  let rec impl (l : string list) : string =
    match l with
    | [] -> ""
    | s :: [] -> s
    | s :: ss -> s ^ ",\n" ^ impl ss
  in "[" ^ impl l ^ "]"

let show_equations (eqs : equation list) : string =
  show_list (List.map (fun (x,y) -> show_term x ^ " = " ^ show_term y) eqs)

let show_sub (env : sub) : string = show_list (List.map (fun (id, term) -> show_id id ^ " ~~>" ^ show_term term) (List.of_seq (IntMap.to_seq env)))

(** Given two neutral-like terms n1 and n2, then if n1 t1 = n2 t2, then t1 = t2 and n1 = n2.*)
let rec neutralLike (t : term) : bool =
  match t with
  | Var _ -> true
  (* | Proj1 _ -> true
  | Proj2 _ -> true
  | Pair _ -> true *)
  | App (t, _) -> neutralLike t
  | MetaData (_, t) -> neutralLike t
  | _ -> false


(** TODO: Terms should be expanded at top level enough to start matching on them (call-by-name)
    They should be reduced until they are a value?
    The idea is that we only need to top level construction to normal?
    Ah - terms should be "pseudoneutral - " that is, a variable (or metavariable) applied to arguments,
    but the arguments don't themselves need to be normal.*)

let rec lift (k : int) (t : term) : term =
  match t with
  | Var i -> if i < k then Var i else Var (i + 1)
  | Lam t -> Lam (lift (k + 1) t)
  | App (t1, t2) -> App (lift k t1, lift k t2)
  | Pair (t1, t2) -> Pair (lift k t1, lift k t2)
  | Proj1 | Proj2 | Const _ | MetaVar _ -> t
  | MetaData (d, t) -> MetaData(d, lift k t)

let rec subst (index : int) (t' : term) (t : term) : term =
  let recur = subst index t' in
  match t with
  | Var i -> if index < i then Var (i - 1) else if i = index then t' else Var i
  | Lam body -> Lam (subst (index + 1) (lift 0 t') body)
  | App (t1, t2) -> App (recur t1, recur t2)
  | Pair (t1, t2) -> Pair (recur t1, recur t2)
  | Proj1 | Proj2 | Const _ | MetaVar _ -> t
  | MetaData (d, t) -> MetaData(d, recur t)

let rec reduceImpl (env : sub) (t : term) : term option =
  match t with
  | MetaVar x when IntMap.mem x env -> (
        let value = IntMap.find x env in
        match reduceImpl env value with
        | None -> Some value
        | Some t -> Some t)
  | Lam (App (t, Var 0)) -> Some t (*function eta*)
  | Pair (App (Proj1, t1), App (Proj2, t2)) when t1 = t2 -> Some t1 (*pair eta*) (* TODO: maybe this only needs to happen if t1 and t2 are variables?*)
  | MetaData (_, t) -> Some t
  | App (Lam t1, t2) -> Some (subst 0 t2 t1)
  | App (Proj1, Pair (t, _)) -> Some t
  | App (Proj2, Pair (_, t)) -> Some t
  | App (Proj1, t) -> (* TODO: there must be a cleaner way to write these cases*)
    (match reduceImpl env t with
    | None -> None
    | Some t1' -> Some (App(Proj1, t1')))
  | App (Proj2, t) ->
    (match reduceImpl env t with
    | None -> None
    | Some t1' -> Some (App(Proj2, t1')))
  | App (t1, t2) ->
    (match reduceImpl env t1 with
    | None -> None
    | Some t1' -> Some (App(t1', t2)))
  | _ -> None

(*reduce returns equations in a form that is a variable or metavariable projn'ed and applied to arguments, or a constant.*)
let rec reduce (env : sub) (t : term) : term =
    let res = match reduceImpl env t with
    | Some t' -> reduce env t'
    | None -> t
    in res

exception Bug of string

(* e.g. x A B != y C D E*)
(*
  Assumes that the input terms are reduced, e.g. they are outputs of "reduce"
  returns true if t1 and t2, applied to any number of args and projn'ed any number of times, are definietly unequal.
  For example, if x and y are different free vars, then
  proj1 (proj2 (x 1 2 3)) != (proj1 y) 4 5 6*)
(* TODO: wouldn't it make more sense to separately reduce off the Apps, and then check the pair together afterwards?*)
let rec reducedAreDefinitelyUnequal (t1 : term) (t2 : term) : bool =
  match t1, t2 with
  | App(Proj1, t1'), t2' | t2', App(Proj1, t1') -> reducedAreDefinitelyUnequal t1' t2'
  | App(Proj2, t1'),t2' | t2', App(Proj2, t1') -> reducedAreDefinitelyUnequal t1' t2'
  | App(t1', _), t2' | t2', App(t1', _) -> reducedAreDefinitelyUnequal t1' t2'
  | Lam(t1'), t2' | t2', Lam(t1') -> reducedAreDefinitelyUnequal t1' t2' (* TODO: is this right? *)
  | Pair(a1, b1), Pair(a2, b2) -> reducedAreDefinitelyUnequal a1 a2 || reducedAreDefinitelyUnequal b1 b2
  | MetaVar _, _ | _, MetaVar _ -> false
  | Var n1, Var n2 when n1 == n2 -> false
  | Const s1, Const s2 when s1 == s2 -> false
  | Proj1, Proj1 | Proj2, Proj2 -> false
  | t1, t2 ->
    match getValueTag t1, getValueTag t2 with (*Note that if t1 or t2 is a lambda, then it can't have been on the left of an App. Likewise, if its a pair, it can't have been in a proj1 or proj2.*)
    | Some tag1, Some tag2 -> not (tag1 = tag2)
    | _ -> raise (Bug ("In reduceAreUnequal, " ^ show_term t1 ^ " and " ^ show_term t2))

(*This implementation is not fast*)
let rec norm (t : term) : term =
  match t with
  | Lam (App (t, Var 0)) -> norm t (*function eta*)
  | Pair (App (Proj1, t1), App (Proj2, t2)) when t1 = t2 -> norm t1 (*pair eta*)
  | MetaData (_, t) -> t
  | App (Lam t1, t2) -> norm (subst 0 t2 t1)
  | App (Proj1, Pair (t, _)) -> norm t
  | App (Proj2, Pair (_, t)) -> norm t
  | App (t1, t2) ->
      let t1' = norm t1 in
      let t2' = norm t2 in
      let res = App (t1', t2') in
      if t1' = t1 && t2 = t2' then res else norm res
  | Lam t -> Lam (norm t)
  | Pair (t1, t2) -> Pair (norm t1, norm t2)
  | _ -> t


exception Failure of equation

(* Here is an idea to improve performance if thats something I ever do:
   Instead of actually doing substitution on the lambda calculus terms (which is very slow),
   define a "term" as a term along with an environment. The environment is a list of terms.
   Also, I can use a Lazy type somewhere, so that the same reduction doesn't need to be done twice.*)

let rec processEq : (equation -> (equation list) option) unifyM =
  fun env eq ->
  (* (match eq with
  | x, y -> print_endline ("In unify, processing: " ^ show_term x ^ " = " ^ show_term y))
  ; *)
  let ifFail = Failure eq in
  match eq with
  | Lam t1, Lam t2 -> Some [(t1, t2)]
  | Var x1, Var x2 -> if x1 = x2 then Some [] else raise ifFail
  | Const s1, Const s2 -> if s1 = s2 then Some [] else raise ifFail
  | MetaVar x, t2 when IntMap.mem x !env -> processEq env (IntMap.find x !env, t2)
  | t1, MetaVar x when IntMap.mem x !env -> processEq env (t1, IntMap.find x !env)
  | MetaVar x , MetaVar x' when x = x' -> Some []
  | MetaVar x, t | t , MetaVar x -> env := IntMap.add x t !env ; Some []
  | (App (n1,t1), App (n2, t2)) when neutralLike n1 && neutralLike n2 -> Some [(n1, n2); (t1, t2)]
  | App (MetaVar x, Var n), App (MetaVar y, Var n') when n = n' -> Some [(MetaVar x, MetaVar y)] (* TODO: I think that this case and the next are redundant with the X x = t case!*)
  | App (MetaVar x, Pair (Var a, Var b)), App (MetaVar y, Pair (Var a', Var b'))
    when a = a' && b = b' && a <> b -> Some [(MetaVar x, MetaVar y)]
  (*| (Lam _, t) | (t, Lam _) when neutralLike t -> raise ifFail*)(*Note to self: I have no memory of what this case is suppposed to be for.*)
  (* | Lam _, Pair (_, _) | Pair (_, _), Lam _ | Lam _, Const _ (* TODO: think of a smarter way to express these situations where you get a contradiction *)
    | Const _ , Lam _ | Const _, Pair (_, _) | Pair (_, _), Const _
    | Lam _, Proj1| Proj1, Lam _| Lam _, Proj2| Proj2, Lam _| Proj1, Proj2| Proj2, Proj1
    | Const _, Proj1| Proj1, Const _| Const _, Proj2| Proj2, Const _
    | Var _, Const _ | Const _ , Var _ | Var _ , Pair(_, _) | Pair(_, _), Var _
    | Proj1, Pair(_, _) | Pair(_, _), Proj1 | Proj2, Pair(_, _) | Pair(_, _), Proj2  -> raise ifFail *)
  | Proj1, Proj1 | Proj2, Proj2 -> Some []
  | Pair (a1, b1), Pair (a2, b2) -> Some [a1,a2; b1,b2]
  | App (t1, Var n), t2 when var_not_occurs !env n t1 -> (*e.g. if A x = t, then A = \x.t*)
    Some [t1, subst n (Var 0) (Lam t2)]
  | t2, App (t1, Var n) when var_not_occurs !env n t1 ->
    Some [subst n (Var 0) (Lam t2), t1]
  | App (t1, Pair (Var n, Var m)), t2 when n <> m && var_not_occurs !env n t1 && var_not_occurs !env m t1 ->
    Some [t1, subst m (App (Proj2, Var 0)) (subst n (App(Proj1, Var 0)) (Lam t2))]
  | t2, App (t1, Pair (Var n, Var m)) when n <> m && var_not_occurs !env n t1 && var_not_occurs !env m t1 ->
    Some [subst m (App (Proj2, Var 0)) (subst n (App(Proj1, Var 0)) (Lam t2)), t1]
  (* TODO: Think about these cases more carefully! *)
  | t1, App (Proj1, t2) | App (Proj1, t2), t1 -> Some [t2, Pair(t1, MetaVar (freshId ()))]
  | t1, App (Proj2, t2) | App (Proj2, t2), t1 -> Some [t2, Pair(MetaVar (freshId ()), t1)]
  | t1, t2 when reducedAreDefinitelyUnequal t1 t2 -> raise ifFail
  | _ -> None

(* Note that the resulting substitution is NOT idempotent *)
let rec unifyImpl : (bool -> equation list -> equation list -> equation list) unifyM =
  fun env madeProgress eqs todo -> match eqs with
    | (t1, t2) :: es -> (
        let e = (reduce !env t1, reduce !env t2) in
        (* let e = (norm (metaSubst !env t1), norm (metaSubst !env t2)) in *)
        (* let (t1, t2) = e in *)
        (* print_endline ("Processing: " ^ show_term t1 ^ " = " ^ show_term t2); *)
        let newEqs = processEq env e in
        match newEqs with
        | None -> unifyImpl env madeProgress es (e :: todo)
        | Some eqs' -> unifyImpl env true (eqs' @ es) todo
        )
    | [] -> if madeProgress then unifyImpl env false todo [] else todo

let unify (eqs:equation list) : (sub * equation list) option =
  try
    let sub = ref IntMap.empty in
    let finalEqs = unifyImpl sub false eqs [] in
    Some (!sub, finalEqs)
  with Failure(t1, t2) -> print_endline ("unify failed with: " ^ show_term t1 ^ " != " ^ show_term t2 ); None

let unifyPartially (env : sub) (eqs : equation list) : (sub * equation list) option =
  try
    let sub = ref env in
    let finalEqs = unifyImpl sub false eqs [] in
    Some (!sub, finalEqs)
  with Failure(_) -> None


let test (eqs : equation list) : (((id * term) list) * equation list) option =
  match unify eqs with
  | None -> None
  | Some (sub, eqs') -> Some (List.of_seq (IntMap.to_seq sub), eqs')