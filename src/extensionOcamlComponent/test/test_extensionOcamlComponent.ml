open ExtensionOcamlComponent.Parsing
open ExtensionOcamlComponent.SpecSpec
open ExtensionOcamlComponent.TypeSystem
open ExtensionOcamlComponent.Typecheck
open ExtensionOcamlComponent.Unification

let times : (float * string) list ref = ref []
let add_time (msg : string) =
  times := (Sys.time () , msg) :: !times

let testSpecSpec (_ : unit) =
  print_endline "Test: ability to load a spec, and use that spec to check a second program.";
  let parserSpec = makeParser spec in
  
  (* The specification for the program *)
  let langSpec =
    {|
      {
        Term Num, Term Num
        ----------------------- "_ + _"
        Term Num
      }

      {
        Term Num, Term Num
        ---------------------- "_ == _"
        Term Bool
      }

      {
        ----------- "5"
        Term Num
      }

      {
        ----------- "true"
        Term Bool
      }

      {
        Term Bool
        ---------- "tonum _"
        Term Num
      }

      {
        Term Num
        ---------- "_"
        Top
      }

      {
        Term ?a
        ------------- "(_)"
        Term ?a
      }

      {
        Term Num, Term Num
        ----------------------- "_ ++ _"
        Term NumX
      }

      {
        Term Num, Term Num
        ----------------------- "_ ++ _"
        Term NumX
      }
    |};
  in

  (* The program to be checked *)
  let program = [
    {|
(tonum ( (5 + 5)== 5))
    |};
  ] in

  add_time "start";

  let topSort = (topLevel nilSort (MetaVar (freshId ()))) in
  add_time "parsing";
  let parsed = doParse parserSpec (fun x -> x) show_term [langSpec]
    topSort
    (fun x y -> Option.is_some (unify [x, y])) in
  add_time "parsed";
  match parsed with
  | Error msg -> print_endline ("Failed to parse spec: " ^ msg)
  | Ok t ->
    (* print_endline "parsed AST of spec:"; *)
    (* print_endline (show_tree show_ast_label_short t); *)
    add_time "checking";
    let typeErrors = typecheck spec topSort t in
    add_time "checked";
    if List.length typeErrors <> 0 then
      (print_endline "Typechecking errors while checking spec:";
      List.iter (fun err -> print_endline (show_errorMessasge err)) typeErrors)
    else
    print_endline "Spec checked correctly, converting to inductive: ";
    add_time "start convert to inductive";
    let lang = specAstToLang t in
    add_time "end convert to inductive";

    let topSort = (Const "Top") in
    let progParser = makeParser lang in (* TODO: This should take sub!*)
    add_time "start parsing prog";
    let parsedProg = doParse progParser (fun x -> x) show_term program
      topSort
      (fun x y -> Option.is_some (unify [x, y]))
      (* (fun _x _y -> true) *)
    in
    add_time "end parsing prog";
    match parsedProg with
    | Error msg -> print_endline ("Failed to parse program: " ^ msg)
    | Ok t ->
      print_endline "Program parsed successfully. parsed AST of program:";
      print_endline (show_tree show_ast_label_short t);
      add_time "start typechecking prog";
      let typeErrors = typecheck lang topSort t in
      add_time "end typechecking prog";
      if List.length typeErrors <> 0 then
        (print_endline "Typechecking errors while checking spec:";
        List.iter (fun err -> print_endline (show_errorMessasge err)) typeErrors)
      else
      print_endline "Program typechecked!";
      ()



let _ =
    testSpecSpec ();
    add_time "done";
    List.iter (fun (t, msg) ->
      print_endline ("At " ^ msg ^ " time " ^ string_of_float t ^ "s")
    ) !times

  (* print_endline (string_of_bool (Option.is_some (unify [App (Const "a", MetaVar (freshId ())), App (Const "b", MetaVar (freshId ()))]))); *)

