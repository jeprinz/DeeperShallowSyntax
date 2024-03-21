open Str;;

let str = "examplestring"

type sort =
  Keyword of string
  | Pattern of regexp
  | Const of string

(* Pattern (Str.regexp {| |})  *)

type label = string

(*TODO: I think these types don't quite make sense. The conclusion should only be a const.*)
type rule = Rule of label * sort * (sort list)
let exampleRules : rule list = [
  Rule ("App", Const "Term", [Const "Term"; Const "Term"]);
  Rule ("Lam", Const "Term", [Keyword "Lam"; Const "Term"]);
  Rule ("Var", Const "Term", [Pattern (Str.regexp {|([A-Za-z]+|})])
]

type tree = Node of label * (tree list) | String of string

type stackElem =
  String of string
  | Tree of sort * tree

type parseState = {
  stack : stackElem list;
  position : int;
}

(* type parseResult =
  InProcess of parseState
  | Success of tree
  | Failure *)

let sane_regex_match (r : regexp) (s : string) : bool =
  Str.string_match r s 0 && Str.matched_string s = s

let matchElem (se : stackElem) (st : sort) : tree option =
    match se, st with
    | String actual, Keyword expected -> if actual = expected then Some (String actual) else None
    | String actual, Pattern expected -> if sane_regex_match expected actual then Some (String actual) else None
    | Tree (actual, t), expected -> if actual = expected then Some t else None
    | _ -> None

(*This lists should be in reverse order from what is written in the Rules
   If it succeeds in a match, it returns the popped stack and the popped children*)
let matchLists (stack : stackElem list) (reverseChildren : sort list) : (stackElem list * tree list) option =
  let rec impl stack reverseChildren (children : tree list)
    = match stack, reverseChildren with
      | _ , [] -> Some (stack, children)
      | s :: stack', child1 :: reverseChildren' ->
          if _ s child1 then
            impl stack' reverseChildren' (_ :: children)
          else None
      | _, _ -> None
  in impl stack reverseChildren []

module StringSet = Set.Make(String);;

type language = rule list
type processedLanguage = {
  reversedRules : rule list;
  keywords : StringSet.t
} 

let processLanguage (lang : language) : processedLanguage =
  {
    reversedRules = List.map (fun (Rule (l, c, hs)) -> Rule (l, c, List.rev hs)) lang;
    keywords = StringSet.of_list (List.concat (List.map (fun (Rule (_, c, hs))) lang));
  }

let parse (input : string) (lang : language) : tree option =
  _

let _ =
  print_endline "this is here"
  ; print_char (String.get str 5)