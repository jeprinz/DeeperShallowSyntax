open ExtensionOcamlComponent.Parsing
open ExtensionOcamlComponent.SpecSpec
open ExtensionOcamlComponent.TypeSystem
open ExtensionOcamlComponent.Typecheck
open ExtensionOcamlComponent.Unification

let testSpecSpec (_ : unit) =
  print_endline "Test: ability to load a spec, and use that spec to check a second program.";
  let parserSpec = makeParser spec in
  
  (* The specification for the program *)
  let langSpec = [
    {|
      {
        Term, Term
        ------------ "_ + _"
        Term
      }

      {
        Term, Term
        ------------ "_ - _"
        Term
      }

      {
        ----------- "x"
        Term
      }

      {
        ----------- "y"
        Term
      }
    |};
  ] in

  (* The program to be checked *)
  let program = [
    (* {|
      x + y - x
    |}; *)
    "x + y - x - x - x - x - x - x - x - x - x - x - x - x";
  ] in
  let topSort = (topLevel nilSort (MetaVar (freshId ()))) in
  let parsed = doParse parserSpec (fun x -> x) show_term langSpec
    topSort
    (fun x y -> Option.is_some (unify [x, y])) in
  match parsed with
  | Error msg -> print_endline ("Failed to parse spec: " ^ msg)
  | Ok t ->
    print_endline "parsed AST of spec:";
    print_endline (show_tree show_ast_label_short t);
    let typeErrors = typecheck spec topSort t in
    if List.length typeErrors <> 0 then
      (print_endline "Typechecking errors while checking spec:";
      List.iter (fun err -> print_endline (show_errorMessasge err)) typeErrors)
    else
    print_endline "Spec checked correctly, converting to inductive: ";
    let lang = specAstToLang t in

    let topSort = (Const "Term") in
    let progParser = makeParser lang in (* TODO: This should take sub!*)
    let parsedProg = doParse progParser (fun x -> x) show_term program
      topSort
      (fun x y -> Option.is_some (unify [x, y]))
    in
    match parsedProg with
    | Error msg -> print_endline ("Failed to parse program: " ^ msg)
    | Ok t ->
      print_endline "Program parsed successfully. parsed AST of program:";
      print_endline (show_tree show_ast_label_short t);
      let typeErrors = typecheck lang topSort t in
      if List.length typeErrors <> 0 then
        (print_endline "Typechecking errors while checking spec:";
        List.iter (fun err -> print_endline (show_errorMessasge err)) typeErrors)
      else
      print_endline "Program typechecked!";
      ()

let _ =
  testSpecSpec ();

  (* print_endline (string_of_bool (Option.is_some (unify [App (Const "a", MetaVar (freshId ())), App (Const "b", MetaVar (freshId ()))]))); *)

