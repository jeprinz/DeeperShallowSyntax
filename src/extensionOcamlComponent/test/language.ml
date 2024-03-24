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
  (*Normal: *)
  Rule ("App", "Term", [SortPattern "Term"; SortPattern "Term"]);
  Rule ("Var", "Term", [RegPattern (Str.regexp {|[A-Za-z]+|})]);
  Rule ("Paren", "Term", [Keyword "("; SortPattern "Term"; Keyword ")"]);
  Rule ("Let", "Term", [Keyword "let"; RegPattern (Str.regexp {|[A-Za-z]+|}); Keyword "="; SortPattern "Term"; Keyword "in"; SortPattern "Term"]);
  Rule ("Plus", "Term", [SortPattern "Term"; Keyword "+"; SortPattern "Term"]);
  Rule ("Number", "Term", [RegPattern (Str.regexp {|[0-9]+|})]);

  (* Rule ("Lam", "Term", [Keyword "fun"; SortPattern "NameList"; Keyword "=>" ; SortPattern "Term"]); *)
  Rule ("Lam", "Term", [Keyword "\\"; SortPattern "NameList"; Keyword "." ; SortPattern "Term"]);
  Rule ("NameNil", "NameList", []);
  Rule ("NameCons", "NameList", [RegPattern (Str.regexp {|[A-Za-z]+|}); SortPattern "NameList"]);

  (*Transformed: *)
  (* Rule ("Var", "Term", [RegPattern (Str.regexp {|[A-Za-z]+|})]);
  Rule ("App-cons", "Term-list", [SortPattern "Term"; SortPattern "Term-list"]);
  Rule ("Plusapp-cons", "Term-list", [Keyword "+"; SortPattern "Term"; SortPattern "Term-list"]);
  Rule ("Paren", "Term", [Keyword "("; SortPattern "Term"; Keyword ")"]);
  Rule ("Term-Nil", "Term-list", []);
  Rule ("Term-oflist", "Term", [SortPattern "Term"; SortPattern "Term-list"]); *)


  (* Rule ("Paren", "Term", [Keyword "("; SortPattern "Term"; Keyword ")"]);
  Rule ("Application", "Term", [RegPattern (Str.regexp {|[A-Za-z]+|}); SortPattern "ArgList"]);
  Rule ("ArgNil", "ArgList", []);
  Rule ("ArgCons", "ArgList", [SortPattern "Term"; SortPattern "ArgList"]); *)

  (*
  (* Rule ("Var", "Atom", [RegPattern (Str.regexp {|[A-Za-z]+|})]); *)
  (* Rule ("Lam", "Term", [Keyword "fun"; RegPattern (Str.regexp {|[A-Za-z]+|}); Keyword "=>" ; SortPattern "Term"]); *)

  (* Rule ("Convert", "Term", [SortPattern "Atom"]); *)
  (* Rule ("Var", "Term", [RegPattern (Str.regexp {|[A-Za-z]+|})]); *)
  (* Rule ("App", "Term", [SortPattern "Term"; SortPattern "Atom"]); *)
  Rule ("Paren", "Atom", [Keyword "("; SortPattern "Term"; Keyword ")"]);
  *)

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