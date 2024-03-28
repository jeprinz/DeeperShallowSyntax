open Parsing
open Unification

(*
In this file, I define what constitutes a description of the type system of a language.
I also define a grammar for parsing such a description.

An example:

example = \a b c. a b

Regex s "[A-Za-z]+"
Term (Cons s gamma a) b 
|------------------------------------- "fun _ => _"
Term gamma (arrow a b)
*)

(* Parser grammar *)

(* type astLabel =
  | RLamArgListNil
  | RLamArgListCons
  | RLamAbs
  | RLamApp
  | RLamVar

type astSort =
  | SLamTerm
  | SLamArgList
  | SLamName

let rules : (astSort, astLabel) language = [
  Rule(RLamAbs, SLamTerm, [Keyword "\\"; SortPattern SLamName ; Keyword "."; SortPattern SLamTerm]);
  Rule(RLamApp, SLamTerm, [SortPattern SLamTerm; SortPattern SLamTerm]);
] *)

(* Representation of a programming language *)

type nameComponent = NameKeyword of string | NameHole
type naming = nameComponent list

type constructor = {
  name : string;
  look : naming;
  premises : term list;
  hiddenPremises : term list;
  conclusion : term;
  equalities : (term * term) list;
  disequalities : (term * term) list;
}

type fullConstructor = {
  boundVars : int list;
  constructor : constructor;
}

module StringMap = Map.Make (String)
let makeRule (ruleDesc : (string -> term) -> constructor) : fullConstructor =
  let varNames = ref StringMap.empty in
  let var name =
    match StringMap.find_opt name !varNames with
    | None -> let i = freshId () in
      varNames := StringMap.add name i !varNames;
      MetaVar i
    | Some i -> MetaVar i
  in
  let constructor = ruleDesc var in
  {
    boundVars = StringMap.fold (fun _ -> List.cons) !varNames [];
    constructor;
  }

let freshenRule (ctr : fullConstructor) : constructor = 
  let mvarMap = List.fold_right (fun x acc -> let y = freshId () in IntMap.add x (MetaVar y) acc) ctr.boundVars IntMap.empty in
  let ren = metaSubst mvarMap in
  let c = ctr.constructor in
  {
    c with
    premises = List.map ren c.premises;
    hiddenPremises = List.map ren c.hiddenPremises;
    conclusion = ren c.conclusion;
    equalities = List.map (fun (x, y) -> ren x, ren y) c.equalities;
    disequalities = List.map (fun (x, y) -> ren x, ren y) c.disequalities;
  }

let regexSort (s : term) (regex : string) =
  App(App(Const "Regex", s), Const regex)
(* TODO: here I am kind of overloading Const to both be constant symbols, and also strings. I should split that into two things. *)
let matchRegexSort (s : term) : (term * string) option =
  match s with
  | App(App(Const "Regex", s), Const regex) -> Some (s , regex)
  | _ -> None

(* let notEqualSort (t1 : term) (t2 : term) =
  App(App(Const "NotEqual", t1), t2)
let matchNotEqualSort (s : term) : (term * term) option =
  match s with
  | App(App(Const "NotEqual", t1), t2) -> Some (t1, t2)
  | _ -> None *)

type inductive = fullConstructor list

type program = string ast

let makeFastConstructorLookup (lang : inductive) : fullConstructor StringMap.t =
  List.fold_right (fun ctr acc -> StringMap.add ctr.constructor.name ctr acc) lang StringMap.empty

(* TODO: deal with the "Regex" special relation. *)

(* Generating a parser for a programming language *)

(* This assumes that the number of children for each naming correctly corresponds to the number of premises *)
let makeParser (lang : inductive) : (term, string) language =
  List.map ( fun ({constructor= {name; look; premises; conclusion; _}; _}) ->
    let i : int ref = ref 0 in
    let pattern = List.map (fun nc ->
      match nc with
      | NameKeyword str -> Keyword str
      | NameHole ->
        let premise = List.nth premises !i in
        i := !i + 1;
        match matchRegexSort premise with
        | None -> SortPattern premise
        | Some (_s, regex) -> RegPattern (Str.regexp regex)
    )look in
    Rule(name, conclusion, pattern)
  ) lang