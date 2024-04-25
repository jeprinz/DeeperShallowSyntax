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
  
  (* The specification for the program *)
  let langSpec =
    {|
nil = Nil
cons = \gamma name ty. (gamma, (name, ty))

zero = snd
succ = \x gamma. x (fst gamma)

pi = \x y. \gamma. Pi (x gamma) (\a. y (gamma, a))
arrow = \x y. pi x (weaken y)
type = \gamma. Type

var = \x. x
lambda = \t. \gamma a. t (gamma, a)
app = \t1 t2. \gamma. (t1 gamma) (t2 gamma)

weaken = \t. \gamma. t (fst gamma)

{
    Regex ?name "[a-zA-Z]+"
    ------------------------- "_"
    Name ?name
}
{
    ----------------------------------------------------- ""
    Var (cons ?Gamma ?name ?T) (weaken ?T) zero ?name
}
{
    Var ?Gamma ?T ?i ?name,
    ?name != ?othername
    -------------------------------------------------------------------- "_"
    Var (cons ?Gamma ?othername ?othertype) (weaken ?T) (succ ?i) ?name
}
{
    Name ?name,
    Term ?Gamma type ?A,
    Term (cons ?Gamma ?name ?A) ?B ?body
    ------------------------------------------ "fun _ : _ => _"
    Term ?Gamma (pi ?A ?B) (lambda ?body)
}
{
    Name ?name,
    Term (cons ?Gamma ?name ?A) ?B ?body
    ------------------------------------------ "fun _ => _"
    Term ?Gamma (pi ?A ?B) (lambda ?body)
}
{
    Term ?Gamma (pi ?A ?B) ?t1,
    Term ?Gamma ?A ?t2
    ---------------------------------------------------------- "_ _"
    Term ?Gamma (\gamma. ?B (gamma, ?t2 gamma)) (app ?t1 ?t2)
}
{
    -------------------------- "Type"
    Term ?Gamma type type
}
{
    Name ?name,
    Term ?G type ?A,
    Term (cons ?G ?name ?A) type ?B,
    ------------------------------------ "(_ : _) -> _"
    Term ?G type (pi ?A ?B)
}
{
    Term ?G type ?A,
    Term ?G type ?B,
    ------------------------------------ "_ -> _"
    Term ?G type (arrow ?A ?B)
}
{
    Term ?G ?T ?t
    --------------- "(_)"
    Term ?G ?T ?t
}
/*
 */
{
    Name ?name1,
    {Var ?ctx ?ty ?t ?name1},
    -------------------------------- "_"
    Term ?ctx ?ty (var ?t)
}
{
    {HoleOf Context ?ctx Type ?ty Term ?t}
    ------------------------- "?"
    Term ?ctx ?ty ?t
}
    |};
  in

  let _langSpec = 
{|{{Test Bla} -"a" (fst (\x y.x, \x y.y)) } |}
  in

  (* The program to be checked *)
  let program = [
    {|
fun A : Type => fun B : Type =>
fun asdf : ? -> ? =>
fun acceptsA : A -> Type =>
fun x : ((f : Type -> (Type -> Type)) -> f A B) =>
    acceptsA (x (fun a => fun b => a))
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
