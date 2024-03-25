open Str;;

(* Throughout this codebase, we work with generic tree *)
type 'label tree = Node of 'label * ('label tree list)

(*Position within a text file*)
type position = {
  lineNumber : int;
  posInLine : int;
}

type 'label astLabel = AstString of string | AstNode of 'label
(* An abstract syntax tree is a tree where each node is either an AstString with no children, or
   a Node with labels from a set.
   Each node also has the beginning and ending position of the node in the text file.*)
type 'label ast = ('label astLabel * position * position) tree


type 'sort pattern =
  Keyword of string
  | RegPattern of regexp
  | SortPattern of 'sort

(* Some show functions *)

let rec show_tree (show_label : 'label -> string) (t : 'label tree) : string =
  match t with
  | Node (l, ts) -> "(" ^ show_label l ^ " " ^ String.concat " " (List.map (show_tree show_label) ts) ^ ")"

let show_position (pos : position) : string =
  "L" ^ string_of_int pos.lineNumber ^ "C" ^ string_of_int pos.posInLine

let show_ast_label ((l, p1, p2) : (string astLabel * position * position)) : string =
  let lString = match l with
    | AstString s -> s
    | AstNode l -> l
  in
  lString ^ show_position p1 ^ "-" ^ show_position p2

(* The parser looks for forms described by rules. Each rule corresponds to a particular kind of node in the
   AST. It consists of a list of patterns, which either describe something that should be present in the text,
   or point to a sort. These sort patterns correspond to child elements of the AST which must be of that sort.*)
type ('sort, 'label) rule = Rule of 'label * 'sort * ('sort pattern list)

type ('sort, 'label) language = ('sort, 'label) rule list

(* The parser uses a top-down backtracking algorithm. It starts at the top of the AST, and fills it in
left to right and top to bottom. If it ever hits a dead end, it backtracks until it finds a working possibility.
Besides this backtracking, it never looks at the input string out of order. The path type describes a partial
AST which has been built up. It resembles a one-hole context, except it is a many-hole context; the rightmost
n children may be missing for each node. In their place, it keeps track of the patterns to be filled in for each
of these partial nodes.  To put it another way, it is a prefix of the nodes of a tree when the tree is viewed in
top-down left-to-right order.
Each node in the path also has the position in the text file for the left end of the node.*)
type ('sort, 'label) path =
  PNode of ('sort, 'label) path * 'label * position * ('label ast list) * ('sort pattern list)
  | Top of 'sort

(* To prevent infinite recursion on left-recursive rules, this function checks any input has been accepted since the last instance of a rule. *)
let rec amILooping (path : ('sort, 'label) path) (ctr : 'label) : bool =
  match path with
  | PNode (above, label, _position, [], _) ->
    (label = ctr) || (amILooping above ctr)
  | _ -> false

let rec skipLeadingWhitespace (lines : string list) (pos : position) : (string list * position) =
  let ifNone = lines, pos in
  match lines with
  | [] -> ifNone
  | line :: lines' ->
    if pos.posInLine = String.length line then
        if lines'= [] then [], pos else skipLeadingWhitespace lines' {posInLine = 0; lineNumber = pos.lineNumber + 1}
    else
    let skipped = Str.string_match (Str.regexp "[ \n\r\x0c\t]+") line pos.posInLine in (*https://stackoverflow.com/questions/39813584/how-to-split-on-whitespaces-in-ocaml*)
    if skipped
      then
        let whitespaceEnd = Str.match_end () in
        skipLeadingWhitespace lines {pos with posInLine = whitespaceEnd}
      else ifNone

(*
This function parses a string according to a list of rules. It returns the AST if the parse was successful.  
The output tree contains labels, and a pair of integers which are the positions in the input string corresponding to that node.
*)
let rec parse (compare : 'sort -> 'sort -> bool) (lang : ('sort, 'label) language)
  (remainingLinesBeforeSpace : string list) (posBeforeSpace : position) (where : ('sort, 'label) path) : ('label ast) option =
  let (remainingLines, pos) = skipLeadingWhitespace remainingLinesBeforeSpace posBeforeSpace in
  let findRule (sort : 'sort) (above : ('sort, 'label) path) =
      List.find_map
        (fun (Rule(newLabel, newSort, newPattern)) ->
          if not (compare newSort sort) || (amILooping above newLabel) then None else
            (parse compare lang remainingLines pos (PNode (above, newLabel, pos, [], newPattern))) 
        )
      lang
  in
  match where with
  (* If we are starting at the top of the AST, find a rule matching the sort *)
  | Top sort -> findRule sort (Top sort)
  (* If there are no more patterns, collect the children into a tree and go up in the path *)
  | PNode (above, label, leftPosition, children, []) ->
      let tree = Node((AstNode label, leftPosition, posBeforeSpace), children) in
      (match above with
        | PNode (above, l, lPos, ts, patterns) -> parse compare lang remainingLines pos (PNode (above, l, lPos, (ts @ [tree]), patterns))
        | Top _sort -> if remainingLines = [] then Some tree else None)
  (* If there are more patterns, then handle the next pattern *)
  | PNode (above, label, lPos, leftChildren, patterns) -> (
    match patterns with
    | SortPattern sort :: patterns' -> findRule sort (PNode (above, label, lPos, leftChildren, patterns'))
    | Keyword expected :: patterns' ->
      if remainingLines = [] then None else
      if String.length (List.hd remainingLines) >= pos.posInLine + (String.length expected) &&
         expected = String.sub (List.hd remainingLines) pos.posInLine (String.length expected)
        then
          let end_position = (pos.posInLine + String.length expected) in
          parse compare lang remainingLines {pos with posInLine = end_position} (PNode(above, label, lPos, leftChildren (*@ [String actual]*), patterns'))
        else None
    | RegPattern expected :: patterns' ->
      if remainingLines = [] then None else
      if Str.string_match expected (List.hd remainingLines) pos.posInLine
        then
          let end_position = {pos with posInLine = Str.match_end ()} in
          parse compare lang remainingLines end_position
            (PNode(above, label, lPos, leftChildren @ [Node((AstString (matched_string (List.hd remainingLines)), pos, end_position), [])], patterns'))
        else None
    | _ -> None
  )

(* In order to deal with left-recursive rules in the grammar, these functions translate the grammar into an alternate form which
   is equivalent but not left-recursive. The idea is to convert left-recursive structure into right-recursive lists.*)

type 'sort internalSort = NormalSort of 'sort | ListSort of 'sort
type 'label internalLabel = NormalLabel of 'label | ConsLabel of 'label | NilLabel | OfListLabel

let rewritePattern (p : 'sort pattern) : 'sort internalSort pattern =
  match p with
  | Keyword s -> Keyword s
  | RegPattern r -> RegPattern r
  | SortPattern s -> SortPattern (NormalSort s)

(* the two special rules for each sort *)
let nilRule (s : 'sort) : ('sort internalSort, 'label internalLabel) rule =
  Rule (NilLabel, ListSort s, [])

let ofListRule (s : 'sort) : ('sort internalSort, 'label internalLabel) rule  =
  Rule(OfListLabel, NormalSort s, [SortPattern (NormalSort s); SortPattern (ListSort s)])

let rewriteRules (compare : 'sort -> 'sort -> bool) (rules : ('sort, 'label) language) : ('sort internalSort, 'label internalLabel) language =
  let convertedRules = List.map (
    fun (Rule(label, sort, patterns)) -> match patterns with
      (* left recursive rule *)
      | SortPattern p :: rest when compare p sort -> Rule(ConsLabel label, ListSort sort, (List.map rewritePattern rest) @ [SortPattern (ListSort sort)])
      (* regular rule *)
      | _ -> Rule(NormalLabel label, NormalSort sort, List.map rewritePattern patterns)
  ) rules in
  let sortsUsedInLeftRecursion = List.filter_map (
    fun (Rule(_label, sort, patterns)) -> match patterns with
      | SortPattern p :: _rest when compare p sort -> Some sort
      | _ -> None
  ) rules in
  convertedRules @ (List.map nilRule sortsUsedInLeftRecursion) @ (List.map ofListRule sortsUsedInLeftRecursion)

let rewriteCompare (compare : 'sort -> 'sort -> bool) : ('sort internalSort -> 'sort internalSort -> bool) =
  fun s1 s2 -> match s1, s2 with
  | NormalSort s1', NormalSort s2' -> compare s1' s2'
  | ListSort s1', ListSort s2' -> compare s1' s2'
  | _ -> false

exception Error of string

(*And these functions convert the resulting transformed ast back into an ast for the original grammar*)
let rec convertBack (t : 'label internalLabel ast) : 'label ast =
  match t with
  | Node ((AstNode OfListLabel, _leftPos, _rightPos), [inside; list]) -> unravelList (convertBack inside) list
  | Node ((AstNode NormalLabel l, leftPos, rightPos), children) -> Node((AstNode l, leftPos, rightPos), List.map convertBack children)
  | Node((AstString s, leftPos, rightPos), []) -> Node((AstString s, leftPos, rightPos), [])
  | _ -> raise (Error "convertBack")

and unravelList (inside : 'label ast) (t : 'label internalLabel ast) : 'label ast =
  match t with
  | Node ((AstNode NilLabel, _, _), []) -> inside
  | Node ((AstNode ConsLabel s, _leftPos, _rightPos), iChildren) -> (
      match inside with Node((_, insideLeftPos, insideRightPos), _) ->
      match List.rev iChildren with
      (* we need the right position to be the right of the rightmost element of iChildren.*)
      | restOfList :: rest ->
        let rightPos = if List.length rest = 0 then insideRightPos else match (List.hd rest) with Node((_, _, rightPos), _) -> rightPos
        in
        unravelList (Node ((AstNode s, insideLeftPos, rightPos), inside :: (List.map convertBack (List.rev rest)))) restOfList
      | _ -> raise (Error "unravelList1")
    )
  | _ -> raise (Error "unravelList")

let doParse (lang : ('sort, 'label) language) (lines : string list) (topSort : 'sort) (compare : 'sort -> 'sort -> bool) : 'label ast option =
  let internalRules = rewriteRules compare lang in
  (* print_endline "internal rules: "; *)
  (* print_endline (String.concat "\n" (List.map show_internalRule internalRules)); *)
  Option.bind (parse (rewriteCompare compare) internalRules lines {lineNumber = 0; posInLine = 0} (Top (NormalSort topSort))) (fun it ->
    (* print_endline ("internal tree was: " ^ show_internalTree it); *)
    Some (convertBack it))

(*
 Note to self:
 This version has a few improvements over the version in the other file:
 1) [x] - It works generically over sorts, labels, and a comparison function. This should make it easier to integrate with other things
 2) [x] - It doesn't assume that the string should be tokenized by whitespace (it nevertheless assumes that whitespace can be skipped)
  I just had a realization - I can make a rule "whitespace : Regex (matches whitespace) any -> any".
  This is equivalent to the "skipWhitespace" function that I wrote!
 3) [x] - I also need to make it keep track of the string positions in the tree.
 4) [x] - I also need to make it automatically skip whitespace.
  I just had a realization - I can make a rule "whitespace : Regex (matches whitespace) any -> any".
  This is equivalent to the "skipWhitespace" function that I wrote!
 5) [x] - I also need to make it input a list of lines rather than just a single string.
 6) [ ] - I also need to make it capable of giving some kind of error message with position when it fails.
        - Maybe keep track of how far it gets in input during backtracking, and assume that the first bit of string that it doesn't ever
          successfully match with anything is the problem?
*)