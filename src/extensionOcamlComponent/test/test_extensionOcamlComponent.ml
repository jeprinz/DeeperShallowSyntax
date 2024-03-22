(* open Bottomup;; *)
open Language;;
open Topdown;;

let _ =
  match (backtrackingParse exampleRules (findKeywords exampleRules) (tokenize "( a ) x ") (Top "Term")) with
  | Some t -> print_endline (show_tree t)
  | None -> print_endline "FAILED"

  (* match (parseTD "Term" "a a a" exampleRules) with
  | Error e -> print_endline e
  | Ok t -> print_endline (show_tree t) *)

  (* match (parse "fun => x x" exampleRules) with
  | Error e -> print_endline e
  | Ok t -> print_endline (show_tree t) *)

  (* let res : string = if (sane_regex_match (Str.regexp {|[A-Za-z]+|}) "a") then "yes" else "no" in *)
  (* print_endline res *)
