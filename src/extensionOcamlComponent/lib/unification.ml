open Stdlib
open Util

(* Higher order unification! *)

type ('s, 'a) state = 's ref -> 'a

type id = int
  [@@deriving show]
let nextId : int ref = ref 0
let freshId (_ : unit) : id =
  let next = !nextId in
  nextId := !nextId + 1
  ; next

type varId = Named of string | Generated of string * int
let show_varId (i : varId) = match i with
  | Named s -> s
  | Generated (name, x) -> "_" ^ name ^ string_of_int x
let varIdName (i : varId) : string =
  match i with
  | Named s -> s
  | Generated (name, _) -> name

type term = MetaVar of id | Lam of varId * term | App of term * term | Var of varId
  | Pair of term * term | Proj1 | Proj2
  | Const of string | MetaData of string * term

let freshMetaVar (_ : unit) : term = (* TODO: use this later on*)
  MetaVar (freshId ())

let freshVarId (name : string) (_ : unit) : varId =
  Generated (name, freshId ())

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
  | Lam(name, body) -> let inside = "λ " ^ show_varId name ^ ". " ^ show_term_impl false false body in
    if lamParens then "(" ^ inside ^ ")" else inside
  | App (t1, t2) ->
      let inside = show_term_impl true false t1 ^ " " ^ show_term_impl true true t2 in
      if appParens then "(" ^ inside ^ ")" else inside
  | Pair (t1, t2) -> "(" ^ show_term_impl false false t1 ^ "," ^ show_term_impl false false t2 ^ ")"
  | Var name -> show_varId name
  | Proj1 -> "fst"
  | Proj2 -> "snd"
  | Const s -> s
  | MetaData (d, t) -> "MD[" ^ d ^ "][" ^ show_term_impl false false t ^ "]"
  | MetaVar x -> "M" ^ show_id x

let show_term = show_term_impl false false

let children (t : term) : term list =
  match t with
  | Lam (_name, t) -> [t]
  | App (t1, t2) -> [t1; t2]
  | Pair (t1, t2) -> [t1; t2]
  | MetaData (_, t) -> [t]
  | _ -> []

module IntMap = Map.Make (Int)
type sub = term IntMap.t 
type subList = (int * term) list (*[@@deriving show]*)

let show_sub (s : sub) : string =
  "{" ^ IntMap.fold (fun key t acc -> string_of_int key ^ " -> " ^ show_term t ^ ", " ^ acc) s "" ^ "}"

(* Assumes that t is closed *)
let rec metaSubst (menv : sub) (t : term) : term =
  let recur = metaSubst menv in
  match t with
  | Lam (name, body) -> Lam (name, recur body)
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

let rec var_not_occurs (env : sub) (x : varId) (t : term) : bool =
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

(* let show_sub (env : sub) : string = show_list (List.map (fun (id, term) -> show_id id ^ " ~~>" ^ show_term term) (List.of_seq (IntMap.to_seq env))) *)

(** Given two neutral-like terms n1 and n2, then if n1 t1 = n2 t2, then t1 = t2 and n1 = n2.*)
(* let rec neutralLike (t : term) : bool =
  match t with
  | Var _ -> true
  | Const _ -> true (*TODO: Is this right?*)
  (* | Proj1 _ -> true
  | Proj2 _ -> true
  | Pair _ -> true *)
  | App (t, _) -> neutralLike t
  | MetaData (_, t) -> neutralLike t
  | _ -> false *)

(** Given two neutral-like terms t1 = (x a1 ... an) and t2 = (y b1 ... bn), then if t1 = t2, x = y and ai = bi.*)
let rec neutralLike (t : term) : (term * term list) option =
  match t with
  | Var _ -> Some (t, [])
  | Const _ -> Some (t, [])
  | App (t1, t2) ->
      Option.bind (neutralLike t1) (fun (x, ts) -> Some (x, ts @ [t2]))
  | MetaData (_, t) -> neutralLike t
  | _ -> None


(** TODO: Terms should be expanded at top level enough to start matching on them (call-by-name)
    They should be reduced until they are a value?
    The idea is that we only need to top level construction to normal?
    Ah - terms should be "pseudoneutral - " that is, a variable (or metavariable) applied to arguments,
    but the arguments don't themselves need to be normal.*)

let rec subst (name : varId) (t' : term) (t : term) : term =
  let recur = subst name t' in
  match t with
  | Var x -> if x = name then t' else t
  | Lam (name', body) ->
    if name' = name then t else
    if not (var_not_occurs IntMap.empty name' t') (* capture avoiding substitution *)
      then
        let freshVar = freshVarId (varIdName name') () in
        let body' = subst name' (Var freshVar) body in
        Lam(freshVar, recur body')
      else Lam (name', recur body)
  | App (t1, t2) -> App (recur t1, recur t2)
  | Pair (t1, t2) -> Pair (recur t1, recur t2)
  | Proj1 | Proj2 | Const _ | MetaVar _ -> t
  | MetaData (d, t) -> MetaData(d, recur t)

type pattern = Var of varId | (* App of pattern * term | *) Pair of pattern * pattern | Proj1 of pattern | Proj2 of pattern
let rec term_of_pattern (p : pattern) : term =
  match p with
  | Var x -> Var x
  | Pair(p1, p2) -> Pair(term_of_pattern p1, term_of_pattern p2)
  | Proj1 p -> App(Proj1, term_of_pattern p)
  | Proj2 p -> App(Proj2, term_of_pattern p)

let rec pattern_of_term_opt (t : term) =
  match t with
  | Var x -> Some (Var x)
  | Pair(t1, t2) -> Option.bind (pattern_of_term_opt t1) (fun p1 -> Option.bind (pattern_of_term_opt t2) (fun p2 -> Some (Pair(p1, p2))))
  | App(Proj1, t) -> Option.bind (pattern_of_term_opt t) (fun p -> Some (Proj1 p))
  | App(Proj2, t) -> Option.bind (pattern_of_term_opt t) (fun p -> Some (Proj2 p))
  | _ -> None

let rec substNeutral (u : pattern) (t' : term) (t : term) : term =
  match u with
  | Var x -> subst x t' t
  | Pair(p1, p2) -> substNeutral p1 (App(Proj1, t')) (substNeutral p2 (App(Proj2, t')) t)
  | Proj1 p -> substNeutral p (Pair(t', App(Proj2, term_of_pattern p))) t
  | Proj2 p -> substNeutral p (Pair(App(Proj1, term_of_pattern p), t')) t

(*
For (x, y), need to check that both x and y don't occur.  
For (fst x), only need to check that (fst x) doesn't occur.

TODO: this function doesn't correctly check for more general patterns.
For example, (pattern_not_occurs _ (fst x) x) should return false, but instead returns true.
But on the other hand, (pattern_not_occurs _ (fst x) (snd x)) should return true.
*)
let rec pattern_not_occurs (env : sub) (p : pattern) (t : term) : bool =
    match p, t with
    | Pair(p1, p2), _ -> pattern_not_occurs  env p1 t && pattern_not_occurs env p2 t
    | _, MetaVar y -> (match IntMap.find_opt y env with
      | Some t' -> pattern_not_occurs env p t'
      | None -> true)
    | Var x, Var y -> not (x = y)
    | Proj1 p, App(Proj1, a) -> pattern_not_occurs env p a
    | Proj2 p, App(Proj2, a) -> pattern_not_occurs env p a
    | _ -> List.for_all (pattern_not_occurs env p) (children t)

let rec reduceImpl (env : sub) (t : term) : term option =
  match t with
  | MetaVar x when IntMap.mem x env -> (
        let value = IntMap.find x env in
        match reduceImpl env value with
        | None -> Some value
        | Some t -> Some t)
  | Lam (x1, App (t, Var x2)) when t <> Proj1 && t <> Proj2 &&  x1 = x2 && var_not_occurs env x2 t -> Some t (*function eta*)
  | Pair (App (Proj1, t1), App (Proj2, t2)) when t1 = t2 -> Some t1 (*pair eta*) (* TODO: maybe this only needs to happen if t1 and t2 are variables?*)
  | MetaData (_, t) -> Some t
  | App (Lam (x, t1), t2) -> Some (subst x t2 t1)
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
  (* | Lam(x, t1'), t2' | t2', Lam(x, t1') -> reducedAreDefinitelyUnequal t1' t2' *) (* What was this case supposed to be? *)
  | Lam(x, t1), Lam(y, t2) -> reducedAreDefinitelyUnequal t1 (subst y (Var x) t2)
  | Pair(a1, b1), Pair(a2, b2) -> reducedAreDefinitelyUnequal a1 a2 || reducedAreDefinitelyUnequal b1 b2
  | MetaVar _, _ | _, MetaVar _ -> false
  | Var n1, Var n2 -> n1 <> n2
  | Const s1, Const s2 -> s1 <> s2
  | Proj1, Proj1 | Proj2, Proj2 -> false
  (* Its tricky to tell if a pair is not equal to something, because it could eta reduce. Obviously, I
     need a better algorithm overall for this stuff. *)
  | Pair(_, _), _ | _, Pair(_, _) -> false
  | t1, t2 ->
    match getValueTag t1, getValueTag t2 with (*Note that if t1 or t2 is a lambda, then it can't have been on the left of an App. Likewise, if its a pair, it can't have been in a proj1 or proj2.*)
    | Some tag1, Some tag2 -> not (tag1 = tag2)
    | _ -> raise (Bug ("In reduceAreUnequal, " ^ show_term t1 ^ " and " ^ show_term t2))

(*This implementation is not fast*)
let rec norm (t : term) : term =
  match t with
  | Lam (x1, App (t, Var x2)) when t <> Proj1 && t <> Proj2 &&  x1 = x2 && var_not_occurs (IntMap.empty) x2 t -> norm t (*function eta*)
  | Pair (App (Proj1, t1), App (Proj2, t2)) when t1 = t2 -> norm t1 (*pair eta*)
  | MetaData (_, t) -> t
  | App (Lam (x, t1), t2) -> norm (subst x t2 t1)
  | App (Proj1, Pair (t, _)) -> norm t
  | App (Proj2, Pair (_, t)) -> norm t
  | App (t1, t2) ->
      let t1' = norm t1 in
      let t2' = norm t2 in
      let res = App (t1', t2') in
      if t1' = t1 && t2 = t2' then res else norm res
  | Lam (x, t) ->
    let t' = norm t in
    let res = Lam(x, t') in
    if t' = t then res else norm res
  | Pair (t1, t2) -> Pair (norm t1, norm t2)
  | _ -> t


exception Failure of equation

(* Here is an idea to improve performance if thats something I ever do:
   Instead of actually doing substitution on the lambda calculus terms (which is very slow),
   define a "term" as a term along with an environment. The environment is a list of terms.
   Also, I can use a Lazy type somewhere, so that the same reduction doesn't need to be done twice.*)

let rec processEq : (equation -> (equation list) option) unifyM =
  fun env (t1Pre, t2Pre) ->
  print_endline ("In processEq, processing: " ^ show_term (metaSubst !env t1Pre) ^ " = " ^ show_term (metaSubst !env t2Pre));
  let eq = (norm (metaSubst !env t1Pre), norm (metaSubst !env t2Pre)) in
  let ifFail = Failure eq in
  match eq with
  | t1, t2 when t1 = t2 (*norm (metaSubst !env t1) = norm (metaSubst !env t2)*) -> Some []
  | Lam (x, t1), Lam (y, t2) -> Some [(t1, subst y (Var x) t2)]
  | Var x1, Var x2 -> if x1 = x2 then Some [] else raise ifFail
  | Const s1, Const s2 -> if s1 = s2 then Some [] else raise ifFail
  | MetaVar x, t2 when IntMap.mem x !env -> processEq env (IntMap.find x !env, t2)
  | t1, MetaVar x when IntMap.mem x !env -> processEq env (t1, IntMap.find x !env)
  | MetaVar x , MetaVar x' when x = x' -> Some []
  (* TODO: There is an issue here. This case should give an error if t has free variables.
     For example, suppose we started with (\x. ?A) = (\x. x). This would simplify to ?A = x.*)
  | MetaVar x, t | t , MetaVar x -> env := IntMap.add x t !env ; Some []
  | t1, t2 when Option.is_some (neutralLike t1) && Option.is_some (neutralLike t2) ->
      Option.bind (neutralLike t1) (fun (x1, args1) ->
        Option.bind (neutralLike t2) (fun (x2, args2) ->
          if not (List.length args1 == List.length args2) then raise ifFail else
          Some ((x1, x2) :: List.map2 (fun arg1 arg2 -> arg1, arg2) args1 args2)
      ))
      (* Some [(n1, n2); (t1, t2)] *)
  | App (MetaVar x, Var n), App (MetaVar y, Var n') when n = n' -> Some [(MetaVar x, MetaVar y)] (* TODO: I think that this case and the next are redundant with the X x = t case!*)
  | App (MetaVar x, Pair (Var a, Var b)), App (MetaVar y, Pair (Var a', Var b'))
    when a = a' && b = b' && a <> b -> Some [(MetaVar x, MetaVar y)]
  | Proj1, Proj1 | Proj2, Proj2 -> Some []
  | Pair (a1, b1), Pair (a2, b2) -> Some [a1,a2; b1,b2]
  (* THis the case I need, but seems to be causing nontermination *)
  (*| Pair(a1, a2), t | t, Pair(a1, a2) ->
    Some[App(Proj1, t), a1; App(Proj2, t), a2] (*Is this right?*)*)
  (* These two cases are redundant with the more general case below, but they preserve readable variable names better. *)
  | App (t1, Var x), t2 when t1 <> Proj1 && t1 <> Proj2 && var_not_occurs !env x t1 -> (*e.g. if A x = t, then A = \x.t*)
    Some [t1, Lam (x, t2)]
  | t2, App (t1, Var x) when t1 <> Proj1 && t1 <> Proj2 && var_not_occurs !env x t1 -> (*e.g. if A x = t, then A = \x.t*)
    Some [t1, Lam (x, t2)]
  (*e.g. if A x = t, then A = \x.t*)
  (* I have a != Proj1 or Proj2, because really Proj1 and Proj2 should take their argument as a parameter directly and shouldn't show up here. The issue is cases like "snd x = ..." will trigger this case.*)
  | App(a, p), t when a <> Proj1 && a <> Proj2 && Option.is_some (Option.bind (pattern_of_term_opt p) (fun p -> if pattern_not_occurs !env p a then Some () else None)) ->
    let pat = Option.get (pattern_of_term_opt p) in
    let x = freshVarId "x" () in
    let rhs = (Lam (x, substNeutral pat (Var x) t)) in
    print_endline ("In the pattern case with " ^ show_term (fst eq) ^ " = " ^ show_term (snd eq) ^ ". Outputting: " ^ show_term a ^ " = " ^ show_term rhs ^ " which normalized = " ^ show_term (norm rhs));
    Some [a, Lam (x, substNeutral pat (Var x) t)]
  | t, App(a, p) when  a <> Proj1 && a <> Proj2 && Option.is_some (Option.bind (pattern_of_term_opt p) (fun p -> if pattern_not_occurs !env p a then Some () else None)) ->
    let pat = Option.get (pattern_of_term_opt p) in
    let x = freshVarId "x" () in
    Some [a, Lam (x, substNeutral pat (Var x) t)]
  (* TODO: Think about these cases more carefully! *)
  (* (* THese cases cause an infinite loop with the above case that splits pairs! *)
  | t1, App (Proj1, t2) | App (Proj1, t2), t1 -> Some [t2, Pair(t1, MetaVar (freshId ()))]
  | t1, App (Proj2, t2) | App (Proj2, t2), t1 -> Some [t2, Pair(MetaVar (freshId ()), t1)]
  *)
  (* I wouldn't need these silly cases if Proj1 and Proj2 took an argument *)
  | Proj1, Lam(x, App(Proj1, Var x')) | Lam(x, App(Proj1, Var x')), Proj1
  | Proj2, Lam(x, App(Proj2, Var x')) | Lam(x, App(Proj2, Var x')), Proj2
    when x = x' -> Some []
  | t1, t2 when reducedAreDefinitelyUnequal t1 t2 -> raise ifFail
  | _ -> None

(* Note that the resulting substitution is NOT idempotent *)
let rec unifyImpl : (bool -> ('metadata * equation) list -> ('metadata * equation) list -> ('metadata * equation) list) unifyM =
  fun env madeProgress eqs todo -> match eqs with
    | (md, (t1, t2)) :: es -> (
        (* print_endline ("In unifyImpl, eq processed is " ^ show_term t1 ^ " = " ^ show_term t2); *)
        let e = (reduce !env t1, reduce !env t2) in
        (* let e = (norm (metaSubst !env t1), norm (metaSubst !env t2)) in *)
        (* let (t1, t2) = e in *)
        (* print_endline ("Processing: " ^ show_term t1 ^ " = " ^ show_term t2); *)
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
    None

let unifyPartially (env : sub) (eqs : ('metadata * equation) list) : (sub * ('metadata * equation) list) option =
  try
    let sub = ref env in
    let finalEqs = unifyImpl sub false eqs [] in
    Some (!sub, finalEqs)
  with Failure(_t1, _t2) ->
    (* print_endline ("--------------------------------------- unify failed with: " ^ show_term t1 ^ " != " ^ show_term t2 ); *)
    None


let test (eqs : equation list) : (((id * term) list) * equation list) option =
  match unify eqs with
  | None -> None
  | Some (sub, eqs') -> Some (List.of_seq (IntMap.to_seq sub), eqs')
