open Str;;

type sort = string

type pattern =
  Keyword of string
  | RegPattern of regexp
  | SortPattern of sort

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
type language = rule list

let exampleRules : rule list = [
  (* Rule ("Var", "Term", [RegPattern (Str.regexp {|[A-Za-z]+|})]); *)

  Rule ("Var", "Atom", [RegPattern (Str.regexp {|[A-Za-z]+|})]);
  Rule ("Lam", "Term", [Keyword "fun"; RegPattern (Str.regexp {|[A-Za-z]+|}); Keyword "=>" ; SortPattern "Term"]);
  Rule ("Paren", "Atom", [Keyword "("; SortPattern "Term"; Keyword ")"]);

  Rule ("Convert", "Term", [SortPattern "Atom"]);
  (* Rule ("Var", "Term", [RegPattern (Str.regexp {|[A-Za-z]+|})]); *)
  Rule ("App", "Term", [SortPattern "Term"; SortPattern "Atom"]);

]

let tokenize (input : string) : string list =
  (* https://stackoverflow.com/questions/39813584/how-to-split-on-whitespaces-in-ocaml *)
  Str.split (Str.regexp "[ \n\r\x0c\t]+") input


let findKeywords (lang : language) : StringSet.t =
  let isKeyword (p : pattern) : string option =
    match p with
    | Keyword s -> Some s
    | _ -> None
  in
  StringSet.of_list (List.concat (List.map (fun (Rule (_label, _sort, hs)) -> List.filter_map isKeyword hs) lang));