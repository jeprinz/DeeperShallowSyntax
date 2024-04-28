open Js_of_ocaml
open ExtensionOcamlComponent.Parsing
open ExtensionOcamlComponent.SpecSpec
open ExtensionOcamlComponent.TypeSystem
open ExtensionOcamlComponent.Typecheck
open ExtensionOcamlComponent.Unification

(* Reference to API: https://ocsigen.org/js_of_ocaml/latest/manual/library*)

let () = print_endline "Hello, World!"

(* The positions are returned as an array of errors.
   Each error is a javascript object of the following form:
   
   {
    lineNumber : int,
    posInLine : int,
    severity : string, // Should be either "error" or "warning"
    message : string
   }
*)

(* type jsErrorMessage = <lineNumber : int; posInLine : int; severity : Js.js_string; message : Js.js_string> Js.t *)

(* class type jsErrorMessage = object
  method leftLineNumber : int
  method leftPosInLine : int
  method rightLineNumber : int
  method rightPosInLine : int
  method severity : Js.js_string
  method message : Js.js_string
end *)

let makeErrorMessage (errorMessage : errorMessage) : Js.Unsafe.any =
  (* (object%js
    val lineNumber = errorMessage.lineNumber
    val posInLine = errorMessage.posInLine
    val severity = Js.string (errorMessage.severity)
    val message = Js.string (errorMessage.message)
  end) *)
  Js.Unsafe.obj [|
      ("leftLineNumber", Js.Unsafe.inject errorMessage.pos.left.lineNumber);
      ("leftPosInLine", Js.Unsafe.inject errorMessage.pos.left.posInLine);
      ("rightLineNumber", Js.Unsafe.inject errorMessage.pos.right.lineNumber);
      ("rightPosInLine", Js.Unsafe.inject errorMessage.pos.right.posInLine);
      ("severity", Js.Unsafe.inject (Js.string "error"));
      ("message", Js.Unsafe.inject (Js.string errorMessage.message));
    |]

let parserSpec = makeParser spec

let checkSpecImpl (text : Js.js_string Js.t) : (string ast, errorMessage list) result =
  (* Firebug.console##log("CHECKSPEC IS RUNNING!"); *)
  let string = Js.to_string text in
  (* Firebug.console##log ("Text was: " ^ string); *)
  let lines = String.split_on_char '\n' string in
  let topSort = (topLevel nilSort (MetaVar (freshId ()))) in
  let parsed = doParse2 parserSpec (fun x -> x) show_term lines
    topSort
    (* (fun x y -> Option.is_some (unify [x, y])) *)
    sortCompare
    in
  Firebug.console##log ("Here3");
  match parsed with
  | Error (pos, msg) ->
    Firebug.console##log("Spec parsing falied");
    Error [{pos= {left=pos; right=pos}; message= msg}]
  | Ok t ->
    (* print_endline "parsed AST of spec:"; *)
    (* print_endline (show_tree show_ast_label_short t); *)
    let typeErrors = typecheck spec topSort t in
    if List.length typeErrors <> 0 then
      (Firebug.console##log "Typechecking errors while checking spec:";
      Error typeErrors)
    else
      Ok t

let checkSpec (text : Js.js_string Js.t) : Js.Unsafe.any Js.js_array Js.t =
  match checkSpecImpl text with
  | Ok _ -> Js.array [||]
  | Error typeErrors ->
      Js.array (Array.of_list (List.map (fun err ->
        makeErrorMessage err)
        typeErrors))

let checkProgram (specText : Js.js_string Js.t) (progText : Js.js_string Js.t) : Js.Unsafe.any Js.js_array Js.t =
  match checkSpecImpl specText with
  | Error _ -> Js.array [| makeErrorMessage {pos= {left= {lineNumber=0; posInLine=0}; right={lineNumber=999999; posInLine=999999}}; message= "Specification file didn't check"} |]
  | Ok t ->
    let lang = specAstToLang t in
    let topSort = freshMetaVar () in
    let progParser = makeParser lang in (* TODO: This should take sub!*)
    let string = Js.to_string progText in
    let program = String.split_on_char '\n' string in
    let parsedProg = doParse2 progParser (fun x -> x) show_term program
      topSort
      (* (fun x y -> Option.is_some (unify [x, y])) *)
      (* (fun _x _y -> true) *)
      sortCompare
    in
    match parsedProg with
    | Error (pos, msg) ->
      Firebug.console##log("Program parsing falied");
      Js.array [|makeErrorMessage {pos= {left=pos; right=pos}; message= msg}|]
    | Ok t ->
      Firebug.console##log "Program parsed successfully.";
      let typeErrors = typecheck lang topSort t in
      Js.array (Array.of_list (List.map makeErrorMessage typeErrors))

(*
 The stuff in this object is available to the typescript code in the VSCode extension.  
*)
let _ =
  Js.export "backend"
    (object%js
      method test x =
          x + 11
      method printMessage (_ : int) =
        print_endline "This doesn't work"
        ; Firebug.console##log "This should print in VSCode console"
      method checkSpec = checkSpec
      method checkProgram = checkProgram
    end)