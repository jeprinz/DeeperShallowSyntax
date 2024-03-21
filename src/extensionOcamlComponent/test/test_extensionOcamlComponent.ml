(* open Bottomup;; *)
open Language;;
open Topdown;;

let _ =
  match (parseTD "Term" "fun x => fun y => ( fun z => q )" exampleRules) with
  | Error e -> print_endline e
  | Ok t -> print_endline (show_tree t)

  (* match (parse "fun => x x" exampleRules) with
  | Error e -> print_endline e
  | Ok t -> print_endline (show_tree t) *)

  (* let res : string = if (sane_regex_match (Str.regexp {|[A-Za-z]+|}) "a") then "yes" else "no" in *)
  (* print_endline res *)
