(* open Str;; *)

open Language;;

(*Now, I'm going try a top-down parser and see if with that its easier to get what I want.*)

let parseTD (topSort : sort) (input : string) (lang : language) : (tree, string) result =
  let tokens = tokenize input in
  let keywords = findKeywords lang in
  let rec impl (sort : sort) (useAll : bool) (tokens : string list) : (tree * string list) option =
    List.find_map
      (fun (Rule(label, rsort, pattern)) ->
        if not (rsort = sort) then None else
        (print_endline ("trying rule " ^ label ^ " with tokens = " ^ (String.concat " " tokens) ^ " and useAll = " ^ string_of_bool useAll);
        Option.bind (matchPattern useAll pattern tokens) (fun (children, tokRest) ->
          if useAll && tokRest <> [] then None else
          Some (Node(label, children), tokRest))))
    lang
  and matchPattern (useAll : bool) (patterns : pattern list) (tokens : string list) : (tree list * string list) option =
    match patterns, tokens with
    | Keyword expected :: patterns', actual :: tokens' ->
        if expected = actual
          then Option.bind (matchPattern useAll patterns' tokens') (fun (treesRest, tokRest) -> Some (String actual :: treesRest, tokRest))
          else None
    | RegPattern regex :: patterns', actual :: tokens' ->
        if not (StringSet.mem actual keywords) && sane_regex_match regex actual
          then Option.bind (matchPattern useAll patterns' tokens') (fun (treesRest, tokRest) ->
            Some (String actual :: treesRest, tokRest))
          else None
    | SortPattern sort :: patterns', tokens' ->
        Option.bind (impl sort (useAll && patterns' = []) tokens') (fun (t, tokens'') ->
          Option.bind (matchPattern useAll patterns' tokens'') (fun (treesRest, tokRest) ->
            Some (t :: treesRest, tokRest)))
    | [], [] -> Some ([], [])
    | [], _ -> if useAll then None else Some([], tokens)
    | _, _ -> None
  in match impl topSort true tokens with
  | Some (t, []) -> Ok t
  | _ -> Error "Fail"

(*
 One idea would be to do more backtracking. If subsequent patterns in matchPattern fail, retry with next matching rule for previous pattern.

 So for example, if we have (A B _) suceeded so far, and then _ can't be filled, we should backtrack the choice
  of B to get (A C D) eventually.

 To implement that, I need to make the parse function completely tail recursive, so that if calls fail it can backtrack.
*)

type path =
  Node of path * label * (tree list) * (pattern list)
  | Top of sort

let rec amILooping (path : path) (ctr : label) : bool =
  match path with
  | Node (above, label, [], _) ->
    print_endline ("ctr is: " ^ ctr ^ " and label is: " ^ label) ;
    (label = ctr) || (amILooping above ctr)
  | _ -> false

let rec lengthPath (path : path) : int =
  match path with
  | Top _ -> 0
  | Node (above, _, _, _) -> 1 + lengthPath above

let moveUp (path : path) (tree : tree) : (path, tree) result =
  match path with
  | Node (above, l, ts, patterns) -> Ok (Node (above, l, (ts @ [tree]), patterns))
  | Top _sort -> Error tree
  
let id : int ref = ref 0

exception Error

let rec backtrackingParse (lang : language) (keywords : StringSet.t) (tokens : string list) (where : path) : tree option =
  if lengthPath where > 100 then raise Error else (*TODO: Get rid of this line.*)
  let findRule sort above =
      List.find_map
        (fun (Rule(newLabel, newSort, newPattern)) ->
          if not (newSort = sort) || (amILooping above newLabel) then None else
            ( let myId = !id in id := !id + 1 ;
              print_endline ("start " ^ string_of_int myId ^ " rule " ^ newLabel ^ " with tokens = " ^ (String.concat " " tokens));
              flush stdout;
            let res = (fun x -> x) (backtrackingParse lang keywords tokens
            (Node (above, newLabel, [], newPattern))) in print_endline ("end " ^ string_of_int myId ^ " end") ; res)
        )
      lang
  in
  match where with
  (* | Top sort,  *)
  | Top sort -> findRule sort (Top sort)
  (* | Top _sort -> if tokens = [] then _ else None *)
  | Node (above, label, children, []) ->
      (match moveUp above (Node(label, children)) with
      | Error t -> if tokens = [] then Some t else None
      | Ok path -> backtrackingParse lang keywords tokens path)
  | Node (above, label, leftChildren, patterns) -> (
    match patterns, tokens with
    | SortPattern sort :: patterns', _ -> findRule sort (Node (above, label, leftChildren, patterns'))
    | Keyword expected :: patterns', actual :: tokens' ->
      if expected = actual then backtrackingParse lang keywords tokens' (Node(above, label, leftChildren @ [String actual], patterns')) else None
    | RegPattern expected :: patterns', actual :: tokens' ->
      (* if not (StringSet.mem actual keywords) && sane_regex_match expected actual then *)
      if sane_regex_match expected actual then
        backtrackingParse lang keywords tokens' (Node(above, label, leftChildren @ [String actual], patterns')) else None
    | _, _ -> None
  )

(* This backtracking parser seems to work! The only issue is, the rules need to be ordered so that it tries leaves first before App.
   Maybe this reordering should be done automatically? Maybe only for left recursive rules?
   An issue also on "( a ) b"*)

(* exception Failure of string *)

(* let rec parseImpl (input : (string list) ref) (sort : sort) : tree =
  _

and matchPatternImpl (input : (string list) ref) (patterns : pattern list) : tree list =
  _ *)