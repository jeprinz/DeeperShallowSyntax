open ExtensionOcamlComponent.Parsing
open ExtensionOcamlComponent.SpecSpec
open ExtensionOcamlComponent.TypeSystem
open ExtensionOcamlComponent.Typecheck
open ExtensionOcamlComponent.Unification
open ExtensionOcamlComponent.Util

(* Test 1 is for a left-recursion & compare not being transitive issue in parsing *)
let _langSpec1 = {|
{
    C2,
    ?any1
    ------ "_ + _"
    C2
}
{
  ------ "f"
  ?any
}
/* hello */
{
    C4,
    ?x2
    ---- "_ -> _"
    C4 
}
    |}

let _prog1 = 
    {|
    f + f + f
    |}

let testSpecSpec (_ : unit) =
  print_endline "Test: ability to load a spec, and use that spec to check a second program.";
  let parserSpec = makeParser spec in

  (*

    NOTE WHERE I LEFT OFF: the equality that I am trying to unify here comes up in the bugged test dependent file.
    But for some reason, the parser of the spec doesn't even terminate.

    Constraint had no solution:
    Var
      (?innerCtx,(acceptsA,\ gamma. Pi (snd (fst gamma)) (\ a. Type)))
      (\ x. Pi (snd (fst (fst x))) (\ a. ?typeToBeInferred ((x,snd gamma),a)))
      ?term
      acceptsA
    
    At the time,
    Var
      (?innerCtx,(acceptsA,\ gamma. Pi (snd (fst gamma)) (\ a. Type)))
      (\ x. Pi (?M70632 ((x,snd gamma),?M71112 (fst x))) (\ a. ?typeToBeInferred ((x,snd gamma),a)))
      M70124 acceptsA )
    
    Compare to:

    Var (cons ?Gamma ?name ?T) (weaken ?T) zero ?name

lhs = 
    \gamma2.
    Var
      (?innerCtx,("acceptsA",\ gamma. Pi (snd (fst gamma)) (\ a. Type)))
      (\ x. Pi (snd (fst (fst x))) (\ a. ?X ((x,snd gamma2),a)))
      ?term
      "acceptsA"
---------------------------- OLD:

cons = \gamma name ty. (gamma, (name, ty))
weaken = \t. \gamma. t (fst gamma)
zero = \x. snd x
lhs =
    \gamma2.
    Var
      (?innerCtx,("acceptsA",\ gamma. Pi (snd (fst gamma)) (\ a. Type)))
      (\ x. Pi (?M70632 ((x,snd gamma2),?M71112 (fst x))) (\ a. ?X ((x,snd gamma2),a)))
      ?term
      "acceptsA"
rhs = 
    \gamma2.
    Var (cons ?Gamma ?name ?T) (weaken ?T) zero ?name

---------------------------------------------
z ?= (((M27486,snd ((z,snd gamma),snd (fst (fst z))))),M27487),M27488)

  **)
  
  (* The specification for the program *)
  let langSpec =
    {|
    cons = \gamma name ty. (gamma, (name, ty))
    weaken = \t. \gamma. t (fst gamma)
    zero = \x. snd x
    lhs =
        \gamma2.
        Var
          (?innerCtx,("acceptsA",\ gamma. Pi (snd (fst gamma)) (\ a. Type)))
          (\ x. Pi (?M70632 ((x,snd gamma2),?M71112 (fst x))) (\ a. ?X ((x,snd gamma2),a)))
          ?term
          "acceptsA"
    rhs = 
        \gamma2.
        Var (cons ?Gamma ?name ?T) (weaken ?T) zero ?name
{
    {Result ?X},
    lhs == rhs
    ------------------------------------- "test2"
    Bla
}
    |};
  in

  (* The program to be checked *)
  let program = [
    {|
test2
    |};
  ] in

  log_time "start";

  let topSort = (topLevel nilSort (MetaVar (freshId ()))) in
  log_time "parsing";
  let parsed = doParse parserSpec (fun x -> x) show_term [langSpec]
    topSort
    (fun x y -> Option.is_some (unify [x, y])) in
  log_time "parsed";
  match parsed with
  | Error msg -> print_endline ("Failed to parse spec: " ^ msg)
  | Ok t ->
    (* print_endline "parsed AST of spec:"; *)
    (* print_endline (show_tree show_ast_label_short t); *)
    log_time "checking";
    let typeErrors = typecheck spec topSort t in
    log_time "checked";
    if List.length typeErrors <> 0 then
      (print_endline "Typechecking errors while checking spec:";
      List.iter (fun err -> print_endline (show_errorMessasge err)) typeErrors)
    else (
      print_endline "Spec checked correctly, converting to inductive: ";
      log_time "start convert to inductive";
      let lang = specAstToLang t in
      log_time "end convert to inductive";
      print_endline ("Conclusion of first constructor: " ^ (show_term ((List.hd lang).constructor.conclusion)));

      let topSort = freshMetaVar () in
      let progParser = makeParser lang in (* TODO: This should take sub!*)
      log_time "start parsing prog";
      let parsedProg = doParse progParser (fun x -> x) show_term program
        topSort
        (fun x y -> Option.is_some (unify [x, y]))
        (* (fun _x _y -> true) *)
      in
      log_time "end parsing prog";
      match parsedProg with
      | Error msg -> print_endline ("Failed to parse program: " ^ msg)
      | Ok t ->
        print_endline "Program parsed successfully. parsed AST of program:";
        print_endline (show_tree show_ast_label_short t);
        log_time "start typechecking prog";
        let typeErrors = typecheck lang topSort t in
        log_time "end typechecking prog";
        if List.length typeErrors <> 0 then
          (print_endline "Typechecking errors while checking spec:";
          List.iter (fun err -> print_endline (show_errorMessasge err)) typeErrors)
        else
        print_endline "Program typechecked!";
        ()
    )
