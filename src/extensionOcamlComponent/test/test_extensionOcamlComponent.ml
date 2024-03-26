open ExtensionOcamlComponent.Parsing

let exampleRules : (string, string) language = [
  (* Rule ("App", "Term", [SortPattern "Term"; SortPattern "Term"]); *)
  (* Rule ("Var", "Term", [RegPattern (Str.regexp {|[A-Za-z]+|})]); *)
  (* Rule ("Paren", "Term", [Keyword "("; SortPattern "Term"; Keyword ")"]); *)
  (* Rule ("Let", "Term", [Keyword "let"; RegPattern (Str.regexp {|[A-Za-z]+|}); Keyword "="; SortPattern "Term"; Keyword "in"; SortPattern "Term"]); *)
  (* Rule ("Plus", "Term", [SortPattern "Term"; Keyword "+"; SortPattern "Term"]); *)
  (* Rule ("Number", "Term", [RegPattern (Str.regexp {|[0-9]+|})]); *)
  (* Rule ("Lam", "Term", [Keyword "fun"; RegPattern (Str.regexp {|[A-Za-z]+|}); Keyword "=>" ; SortPattern "Term"]); *)

  Rule ("Number", "Term", [RegPattern (Str.regexp {|[0-9]+|})]);
  Rule ("Plus", "Term", [SortPattern "Term"; Keyword "+"; SortPattern "Term"]);
  Rule ("Paren", "Term", [Keyword "("; SortPattern "Term"; Keyword ")"]);

]

(* let rec tree_size (t : 'label tree) : int =
  match t with
  | Node(_, children) -> 1 + List.fold_right (fun x y -> x + y) (List.map tree_size children) 0 *)

let _ =
  match (doParse exampleRules ["(1 + 2) + 3"] "Term" (fun x y -> x = y)) with
  | Ok t -> print_endline (show_tree show_ast_label t)
  (* | Ok t -> print_endline (string_of_int (tree_size t)) *)
  | Error msg -> print_endline msg