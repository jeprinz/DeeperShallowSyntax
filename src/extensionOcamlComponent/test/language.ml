open Str;;

type sort = string

type pattern =
  Keyword of sort
  | RegPattern of regexp
  | SortPattern of string

(* Pattern (Str.regexp {| |})  *)

type label = string


type tree = Node of label * (tree list) | String of string

let rec show_tree (t : tree) : string =
  match t with
  | String s -> s
  | Node (l, ts) -> "(" ^ l ^ " " ^ String.concat " " (List.map show_tree ts) ^ ")"

type stackElem =
  StringElem of string
  | Tree of sort * tree

let sane_regex_match (r : regexp) (s : string) : bool =
  Str.string_match r s 0 && Str.matched_string s = s

module StringSet = Set.Make(String);;

type rule = Rule of label * sort * (pattern list)

let exampleRules : rule list = [
  Rule ("App", "Term", [SortPattern "Term"; SortPattern "Term"]);
  (* Rule ("Lam", "Term", [Keyword "fun"; RegPattern (Str.regexp {|[A-Za-z]+|}); Keyword "=>" ; SortPattern "Term"]); *)
  Rule ("Lam", "Term", [Keyword "fun"; Keyword "=>" ; SortPattern "Term"]);
  Rule ("Var", "Term", [RegPattern (Str.regexp {|[A-Za-z]+|})])
]