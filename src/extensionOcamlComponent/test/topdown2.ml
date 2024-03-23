open Language;;
open Str;;

type internalSort = NormalSort of string | ListSort of string
let show_internalSort (is : internalSort) : string =
  match is with
  | NormalSort s -> s
  | ListSort s -> "List-" ^ s

type internalPattern =
  Keyword of string
  | RegPattern of regexp
  | SortPattern of internalSort

let show_internalPattern (ip : internalPattern) : string =
  match ip with
  | Keyword s -> s
  | RegPattern _regexp -> "regexp"
  | SortPattern s -> show_internalSort s

type internalLabel = NormalLabel of string | ConsLabel of string | NilLabel | OfListLabel
let show_internal_label (l : internalLabel) : string =
  match l with
  | NormalLabel l -> l
  | ConsLabel l -> "Cons-"^ l
  | NilLabel -> "NilLabel"
  | OfListLabel -> "OfListLabel"

type internalRule = IRule of internalLabel * internalSort * (internalPattern list)
let show_internalRule (IRule(l, s, ps) : internalRule) : string =
  show_internal_label l ^ " " ^ String.concat " " (List.map show_internalPattern ps) ^ " |- " ^ show_internalSort s

(* the two special rules for each sort *)
let nilRule (s : sort) : internalRule =
  IRule (NilLabel, ListSort s, [])

let ofListRule (s : sort) : internalRule =
  IRule(OfListLabel, NormalSort s, [SortPattern (NormalSort s); SortPattern (ListSort s)])

type internalLang = internalRule list

type internalTree = Node of internalLabel * (internalTree list) | String of string

let rec show_internalTree (t : internalTree) : string =
  match t with
  | String s -> s
  | Node (l, ts) -> "(" ^ show_internal_label l ^ " " ^ String.concat " " (List.map show_internalTree ts) ^ ")"

let rewritePattern (p : pattern) : internalPattern =
  match p with
  | Keyword s -> Keyword s
  | RegPattern r -> RegPattern r
  | SortPattern s -> SortPattern (NormalSort s)

let rewriteRules (rules : language) : internalLang =
  List.map (
    fun (Rule(label, sort, patterns)) -> match patterns with
      (* left recursive rule *)
      | SortPattern p :: rest when p = sort -> IRule(ConsLabel label, ListSort sort, (List.map rewritePattern rest) @ [SortPattern (ListSort sort)])
      (* regular rule *)
      | _ -> IRule(NormalLabel label, NormalSort sort, List.map rewritePattern patterns)
  ) rules

exception Error of string

let rec convertBack (t : internalTree) : tree =
  match t with
  | Node (OfListLabel, [inside; list]) -> unravelList (convertBack inside) list
  | Node (NormalLabel l, children) -> Node(l, List.map convertBack children)
  | String s -> String s
  | _ -> raise (Error "convertBack")

and unravelList (inside : tree) (t : internalTree) : tree =
  match t with
  | Node (NilLabel, []) -> inside
  | Node (ConsLabel s, iChildren) -> (
      match List.rev iChildren with
      | restOfList :: rest -> unravelList (Node (s, inside :: (List.map convertBack (List.rev rest)))) restOfList
      | _ -> raise (Error "unravelList1")
    )
  | _ -> raise (Error "unravelList")


type path =
  Node of path * internalLabel * (internalTree list) * (internalPattern list)
  | Top of internalSort

let rec lengthPath (path : path) : int =
  match path with
  | Top _ -> 0
  | Node (above, _, _, _) -> 1 + lengthPath above

let moveUp (path : path) (tree : internalTree) : (path, internalTree) result =
  match path with
  | Node (above, l, ts, patterns) -> Ok (Node (above, l, (ts @ [tree]), patterns))
  | Top _sort -> Error tree

let rec amILooping (path : path) (ctr : internalLabel) : bool =
  match path with
  | Node (above, label, [], _) ->
    print_endline ("amILooping did something: ctr is: " ^ show_internal_label ctr ^ " and label is: " ^ show_internal_label label) ;
    (label = ctr) || (amILooping above ctr)
  | _ -> false
  
let id : int ref = ref 0

let rec parseImpl (lang : internalLang) (keywords : StringSet.t) (tokens : string list) (where : path) : internalTree option =
  if lengthPath where > 100 then raise (Error "parseImpl") else (*TODO: Get rid of this line.*)
  let findRule (sort : internalSort) (above : path) =
      let specialRules = match sort with
        | NormalSort s -> [ofListRule s]
        | ListSort s -> [nilRule s]
      in
      List.find_map
        (fun (IRule(newLabel, newSort, newPattern)) ->
          if not (newSort = sort) || (amILooping above newLabel) then None else
            ( let myId = !id in id := !id + 1 ;
              print_endline ("start " ^ string_of_int myId ^ " rule " ^ show_internal_label newLabel ^ " with tokens = " ^ (String.concat " " tokens));
              flush stdout;
            let res = (fun x -> x) (parseImpl lang keywords tokens
            (Node (above, newLabel, [], newPattern))) in print_endline ("end " ^ string_of_int myId ^ " end") ; res)
        )
      (lang @ specialRules)
  in
  match where with
  (* | Top sort,  *)
  | Top sort -> findRule sort (Top sort)
  (* | Top _sort -> if tokens = [] then _ else None *)
  | Node (above, label, children, []) ->
      (match moveUp above (Node(label, children)) with
      | Error t -> if tokens = [] then Some t else None
      | Ok path -> parseImpl lang keywords tokens path)
  | Node (above, label, leftChildren, patterns) -> (
    match patterns, tokens with
    | SortPattern sort :: patterns', _ -> findRule sort (Node (above, label, leftChildren, patterns'))
    | Keyword expected :: patterns', actual :: tokens' ->
      if expected = actual then parseImpl lang keywords tokens' (Node(above, label, leftChildren (*@ [String actual]*), patterns')) else None
    | RegPattern expected :: patterns', actual :: tokens' ->
      (* if not (StringSet.mem actual keywords) && sane_regex_match expected actual then *)
      if sane_regex_match expected actual then
        parseImpl lang keywords tokens' (Node(above, label, leftChildren @ [String actual], patterns')) else None
    | _, _ -> None
  )

let topDownParse2 (lang : language) (keywords : StringSet.t) (tokens : string list) (topSort : sort) : tree option =
  let internalRules = rewriteRules lang in
  print_endline "internal rules: ";
  print_endline (String.concat "\n" (List.map show_internalRule internalRules));
  Option.bind (parseImpl internalRules keywords tokens (Top (NormalSort topSort))) (fun it ->
    print_endline ("internal tree was: " ^ show_internalTree it);
    Some (convertBack it))