open Str;;

(* Type definitions *)

(*
type label = string
type tree = Node of label * (tree list) | String of string

module type Sort = sig
  type t
  val compare : t -> t -> bool
end

module WithSort(Sort : Sort) = struct
  let bla : Sort.t -> int = fun _ -> 2

  type pattern =
    Keyword of string
    | RegPattern of regexp
    | SortPattern of Sort.t

  type rule = Rule of label * Sort.t * (pattern list)
  type language = rule list
end

(* Show functions *)

let rec show_tree (t : tree) : string =
  match t with
  | String s -> s
  | Node (l, ts) -> "(" ^ l ^ " " ^ String.concat " " (List.map show_tree ts) ^ ")"

module InternalSort = struct
  type t = NormalSort of string | ListSort of string
  let compare t1 t2 = t1 = t2
end

module ExternalSort = struct
  type t = string
  let compare t1 t2 = t1 = t2
end

module WithExternalSort = WithSort(ExternalSort)
module WithInternalSort = WithSort(InternalSort)

let rewritePattern (p : WithExternalSort.pattern) : WithInternalSort.pattern =
  match p with
  | Keyword s -> Keyword s
  | RegPattern r -> RegPattern r
  | SortPattern s -> SortPattern (InternalSort.NormalSort s)

(*
let nilRule (s : sort) : internalRule =
  IRule (NilLabel, ListSort s, [])

let ofListRule (s : sort) : internalRule =
  IRule(OfListLabel, NormalSort s, [SortPattern (NormalSort s); SortPattern (ListSort s)])

type internalLang = internalRule list


let rewriteRules (rules : language) : internalLang =
  List.map (
    fun (Rule(label, sort, patterns)) -> match patterns with
      (* left recursive rule *)
      | SortPattern p :: rest when p = sort -> IRule(ConsLabel label, ListSort sort, (List.map rewritePattern rest) @ [SortPattern (ListSort sort)])
      (* regular rule *)
      | _ -> IRule(NormalLabel label, NormalSort sort, List.map rewritePattern patterns)
  ) rules
*)

*)
type 'label tree = Node of 'label * ('label tree list)

type 'label astLabel = AstString of string | AstNode of 'label
(* type 'label ast = ('label astLabel * int * int) tree *)
type 'label ast = 'label astLabel tree

type 'sort pattern =
  Keyword of string
  | RegPattern of regexp
  | SortPattern of 'sort
type ('sort, 'label) rule = Rule of 'label * 'sort * ('sort pattern list)
type ('sort, 'label) language = ('sort, 'label) rule list

type ('sort, 'label) path =
  PNode of ('sort, 'label) path * 'label * ('label ast list) * ('sort pattern list)
  | Top of 'sort

let rec amILooping (path : ('sort, 'label) path) (ctr : 'label) : bool =
  match path with
  | PNode (above, label, [], _) ->
    (* print_endline ("amILooping did something: ctr is: " ^ show_internal_label ctr ^ " and label is: " ^ show_internal_label label) ; *)
    (label = ctr) || (amILooping above ctr)
  | _ -> false

(*
This function parses a string according to a list of rules. It returns the AST if the parse was successful.  
The output tree contains labels, and a pair of integers which are the positions in the input string corresponding to that node.
*)
let rec parse (compare : 'sort -> 'sort -> bool) (lang : ('sort, 'label) language)
  (input : string) (position : int) (where : ('sort, 'label) path) : ('label ast) option =
  let findRule (sort : 'sort) (above : ('sort, 'label) path) =
      List.find_map
        (fun (Rule(newLabel, newSort, newPattern)) ->
          if not (compare newSort sort) || (amILooping above newLabel) then None else
            (parse compare lang input position (PNode (above, newLabel, [], newPattern))) 
        )
      lang
  in
  match where with
  | Top sort -> findRule sort (Top sort)
  | PNode (above, label, children, []) ->
      let tree = Node(AstNode label, children) in
      (match above with
        | PNode (above, l, ts, patterns) -> parse compare lang input position (PNode (above, l, (ts @ [tree]), patterns))
        | Top _sort -> if position = String.length input then Some tree else None)
  | PNode (above, label, leftChildren, patterns) -> (
    match patterns with
    | SortPattern sort :: patterns' -> findRule sort (PNode (above, label, leftChildren, patterns'))
    | Keyword expected :: patterns' ->
      if expected = String.sub input position (String.length expected)
        then
          let end_position = (position + String.length expected) in
          parse compare lang input end_position (PNode(above, label, leftChildren (*@ [String actual]*), patterns'))
        else None
    | RegPattern expected :: patterns' ->
      if Str.string_match expected input position
        then
          let end_position = Str.match_end () in
          parse compare lang input end_position (PNode(above, label, leftChildren @ [Node(AstString (matched_string input), [])], patterns'))
        else None
    | _ -> None
  )

(*
 Note to self:
 This version has a few improvements over the version in the other file:
 1) It works generically over sorts, labels, and a comparison function. This should make it easier to integrate with other things
 2) It doesn't assume that the string should be tokenized by whitespace  
 3) I also need to make it keep track of the string positions in the tree.
 4) I also need to make it automatically skip whitespace.
 5) I also need to make it input a list of lines rather than just a single string.
 6) I also need to make it capable of giving some kind of error message with position when it fails.
        - Maybe keep track of how far it gets in input during backtracking, and assume that the first bit of string that it doesn't ever
          successfully match with anything is the problem?
*)