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

type astLabel =
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
]

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

type inductive = constructor list

type program = string ast

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
        SortPattern premise
    )look in
    Rule(name, conclusion, pattern)
  ) lang