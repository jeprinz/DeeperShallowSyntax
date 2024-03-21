open Str;;

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

(* exception Failure of string *)

(* let rec parseImpl (input : (string list) ref) (sort : sort) : tree =
  _

and matchPatternImpl (input : (string list) ref) (patterns : pattern list) : tree list =
  _ *)