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
}

let regexSort (s : term) (regex : string) =
  App(App(Const "Regex", s), Const regex)
(* TODO: here I am kind of overloading Const to both be constant symbols, and also strings. I should split that into two things. *)
let matchRegexSort (s : term) : (term * string) option =
  match s with
  | App(App(Const "Regex", s), Const regex) -> Some (s , regex)
  | _ -> None

let notEqualSort (t1 : term) (t2 : term) =
  App(App(Const "NotEqual", t1), t2)
let matchNotEqualSort (s : term) : (term * term) option =
  match s with
  | App(App(Const "NotEqual", t1), t2) -> Some (t1, t2)
  | _ -> None

type inductive = constructor list

type program = string ast

module StringMap = Map.Make (String)
let makeFastConstructorLookup (lang : inductive) : constructor StringMap.t =
  List.fold_right (fun ctr acc -> StringMap.add ctr.name ctr acc) lang StringMap.empty

(* TODO: deal with the "Regex" special relation. *)

(* Generating a parser for a programming language *)

(* This assumes that the number of children for each naming correctly corresponds to the number of premises *)
let makeParser (lang : inductive) : (term, string) language =
  List.map ( fun ({name; look; premises; conclusion; _}) ->
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