(* open Bottomup;; *)
(* open Language;; *)
(* open Topdown;; *)
(* open Topdown2 *)
open Parsing

let exampleRules : (string, string) language = [
  Rule ("App", "Term", [SortPattern "Term"; SortPattern "Term"]);
  Rule ("Var", "Term", [RegPattern (Str.regexp {|[A-Za-z]+|})]);
  Rule ("Paren", "Term", [Keyword "("; SortPattern "Term"; Keyword ")"]);
  Rule ("Let", "Term", [Keyword "let"; RegPattern (Str.regexp {|[A-Za-z]+|}); Keyword "="; SortPattern "Term"; Keyword "in"; SortPattern "Term"]);
  Rule ("Plus", "Term", [SortPattern "Term"; Keyword "+"; SortPattern "Term"]);
  Rule ("Number", "Term", [RegPattern (Str.regexp {|[0-9]+|})]);
  Rule ("Lam", "Term", [Keyword "fun"; RegPattern (Str.regexp {|[A-Za-z]+|}); Keyword "=>" ; SortPattern "Term"]);
]

let _ =
  (* let p = "let f = \\ x y . x + y in f 1 2" in *)
  (* let q = "a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a " in *)
  (* let qqq = q ^ q ^ q ^ q ^ q ^ q in *)

  match (doParse exampleRules ["a";"b";"c"] "Term" (fun x y -> x = y)) with
  | Some t -> print_endline (show_tree show_ast_label t)
  | None -> print_endline "FAILED"

  (* match (topDownParse2 exampleRules (findKeywords exampleRules) (tokenize qqq) "Term") with
  | Some t -> print_endline (show_tree t)
  | None -> print_endline "FAILED" *)

  (* match (backtrackingParse exampleRules (findKeywords exampleRules) (tokenize p) (Top "Term")) with
  | Some t -> print_endline (show_tree t)
  | None -> print_endline "FAILED" *)

  (* match (parseTD "Term" "a a a" exampleRules) with
  | Error e -> print_endline e
  | Ok t -> print_endline (show_tree t) *)

  (* match (parse "fun => x x" exampleRules) with
  | Error e -> print_endline e
  | Ok t -> print_endline (show_tree t) *)

  (* let res : string = if (sane_regex_match (Str.regexp {|[A-Za-z]+|}) "a") then "yes" else "no" in *)
  (* print_endline res *)
