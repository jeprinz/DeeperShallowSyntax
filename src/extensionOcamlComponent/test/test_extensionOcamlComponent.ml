open ExtensionOcamlComponent.Parsing
open ExtensionOcamlComponent.SpecSpec
open ExtensionOcamlComponent.TypeSystem
open ExtensionOcamlComponent.Typecheck
open ExtensionOcamlComponent.Unification

(* let exampleRules : (string, string) language = [
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

] *)

(* let rec tree_size (t : 'label tree) : int =
  match t with
  | Node(_, children) -> 1 + List.fold_right (fun x y -> x + y) (List.map tree_size children) 0 *)

(* let exampleProgram = {|

|} *)

let testSpecSpec (_ : unit) =
  let parserSpec = makeParser spec in
  let code = [
    (* "f = \\ a . a"; *)
    (* "g = \\ x . fa "; *)
    (* "???"; *)
    (* "f = \\x. \\y. \\z. x"; *)
    {|
      f = gg
      g = f
    |};
  ] in
  (* let topSort = (topLevel (MetaVar (freshId ())) (MetaVar (freshId ()))) in *)
  let topSort = (topLevel nilSort (MetaVar (freshId ()))) in
  let parsed = doParse parserSpec code
    topSort
    (fun x y -> Option.is_some (unify [x, y])) in
  match parsed with
  | Error msg -> print_endline msg
  | Ok t ->
    print_endline "parsed AST:";
    print_endline (show_tree show_ast_label t);
    let typeErrors = typecheck spec topSort t in
    print_endline "errors:";
    List.iter (fun err -> print_endline (show_errorMessasge err)) typeErrors

let _ =
  (* match (doParse exampleRules ["(1 + 2) + 3"] "Term" (fun x y -> x = y)) with
  | Ok t -> print_endline (show_tree show_ast_label t)
  | Error msg -> print_endline msg *)

  testSpecSpec ();

  (* let mv = (MetaVar (freshId ())) in
  let t1 = topLevel nilSort (MetaVar (freshId ()))  in
  let t2 =  topLevel mv mv in
  print_endline ("Testing unification on " ^ show_term t1 ^ " and " ^ show_term t2);
  match unify [t1 , t2] with
  | None -> print_endline "fail"
  | Some (sub, eq) ->
    print_endline ("eqs left: " ^ string_of_int (List.length eq));
    print_endline ("sub: " ^ show_sub sub) *)

  (* print_endline (string_of_bool (Option.is_some (unify [App (Const "a", MetaVar (freshId ())), App (Const "b", MetaVar (freshId ()))])));
  print_endline (string_of_bool (Option.is_some (unify [Pair (Const "a", MetaVar (freshId ())), Pair (Const "b", MetaVar (freshId ()))])));
  print_endline (string_of_bool (Option.is_some (unify [topLevel (MetaVar (freshId ())) (MetaVar (freshId ())), termSort (MetaVar (freshId ()))]))) *)