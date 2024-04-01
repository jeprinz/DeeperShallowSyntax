open ExtensionOcamlComponent.Parsing
open ExtensionOcamlComponent.SpecSpec
open ExtensionOcamlComponent.TypeSystem
open ExtensionOcamlComponent.Typecheck
open ExtensionOcamlComponent.Unification

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

let exampleProgram = {|

|}

let testSpecSpec =
  let parserSpec = makeParser spec in
  let code = [
    "f = \\ a . b";
    "g = x";
  ] in
  let parsed = doParse parserSpec code
    (topLevel (MetaVar (freshId ())) (MetaVar (freshId ())))
    (fun x y -> Option.is_some (unify [x, y])) in
  match parsed with
  | Ok t -> print_endline (show_tree show_ast_label t)
  | Error msg -> print_endline msg

let _ =
  (* match (doParse exampleRules ["(1 + 2) + 3"] "Term" (fun x y -> x = y)) with
  | Ok t -> print_endline (show_tree show_ast_label t)
  | Error msg -> print_endline msg *)

  testSpecSpec;

  print_endline (string_of_bool (Option.is_some (unify [App (Const "a", MetaVar (freshId ())), App (Const "b", MetaVar (freshId ()))])));
  print_endline (string_of_bool (Option.is_some (unify [Pair (Const "a", MetaVar (freshId ())), Pair (Const "b", MetaVar (freshId ()))])));
  print_endline (string_of_bool (Option.is_some (unify [topLevel (MetaVar (freshId ())) (MetaVar (freshId ())), termSort (MetaVar (freshId ()))])))