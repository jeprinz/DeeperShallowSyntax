(* open Bottomup;; *)
open Language;;
(* open Topdown;; *)
open Topdown2

let _ =
  (* let p = "( a ) x" in *)
  (* let p = "a a" in *)
  (* let p = "a ( b c + d ) e" in *)
  let p = "fun fun fun => fun fun fun" in

  match (topDownParse2 exampleRules (findKeywords exampleRules) (tokenize p) "Term") with
  | Some t -> print_endline (show_tree t)
  | None -> print_endline "FAILED"

  (* match (backtrackingParse exampleRules (findKeywords exampleRules) (tokenize p) (Top "Term")) with
  | Some t -> print_endline (show_tree t)
  | None -> print_endline "FAILED" *)

  (* match (parseTD "Term" "a a a" exampleRules) with
  | Error e -> print_endline e
  | Ok t -> print_endline (show_tree t) *)

  (* match (parse "fun => x x" exampleRules) with
  | Error e -> print_endline e
  | Ok t -> print_endline (show_tree t) *)

  (* let res : string = if (sane_regex_match (Str.regexp {|[A-Za-z]+|}) "a") then "yes" else "no" in *)
  (* print_endline res *)
