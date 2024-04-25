(* open ExtensionOcamlComponent.Parsing *)
(* open ExtensionOcamlComponent.SpecSpec *)
(* open ExtensionOcamlComponent.TypeSystem *)
(* open ExtensionOcamlComponent.Typecheck *)
open ExtensionOcamlComponent.Unification2
open ExtensionOcamlComponent.Util


let _ =
    (* testSpecSpec (); *)
    let x : term = Node(Var 0, []) in
    let t1 : term = Node(App, [Node(App, [Node(Var 0, []); Node(Const "B", [])]); Node(App, [Node (Var 0, []); Node(Const "B", [])])]) in
    let args = [Node(App, [x; Node(Const "A", [])]); Node(App, [x; Node(Const "B", [])])] in
    (* let args = [Node(App, [x; Node(Const "A", [])])] in *)
    match termsAreUniqueNeutrals args with
    | None -> print_endline "Failed 1"
    | Some args' ->
    (* print_endline ("args' " ^ String.concat ", " (List.map (fun (_, t) -> "["^ String.concat ", " (List.map show_term t) ^ "]") args')); *)
    match reverseSubstituteNeutralForms args' t1 with
    | None -> print_endline "Failed"
    | Some t -> print_endline ("Succeeded: " ^ show_term t)