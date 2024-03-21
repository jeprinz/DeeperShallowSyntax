open Bottomup;;
open Language;;

let _ =
  match (parse "fun => x x" exampleRules) with
  | Error e -> print_endline e
  | Ok t -> print_endline (show_tree t)
  (* let res : string = if (sane_regex_match (Str.regexp {|[A-Za-z]+|}) "hel1lo") then "yes" else "no" in *)
  (* print_endline res *)
