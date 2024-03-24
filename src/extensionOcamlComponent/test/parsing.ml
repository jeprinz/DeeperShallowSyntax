open Str;;

(* Type definitions *)

(* Throughout this codebase, we work with generic tree *)
type 'label tree = Node of 'label * ('label tree list)

type 'label astLabel = AstString of string | AstNode of 'label
(* An abstract syntax tree is a tree where each node is either an AstString with no children, or
   a Node with labels from a set *)
type 'label ast = 'label astLabel tree

type 'sort pattern =
  Keyword of string
  | RegPattern of regexp
  | SortPattern of 'sort

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
top-down left-to-right order.*)
type ('sort, 'label) path =
  PNode of ('sort, 'label) path * 'label * ('label ast list) * ('sort pattern list)
  | Top of 'sort

(* To prevent infinite recursion on left-recursive rules, this function checks any input has been accepted since the last instance of a rule. *)
let rec amILooping (path : ('sort, 'label) path) (ctr : 'label) : bool =
  match path with
  | PNode (above, label, [], _) ->
    (label = ctr) || (amILooping above ctr)
  | _ -> false

let rec skipLeadingWhitespace (lines : string list) (pos : int) : (string list * int) =
  let ifNone = lines, pos in
  match lines with
  | [] -> ifNone
  | line :: lines' ->
    if pos = String.length line then skipLeadingWhitespace lines' 0 else
    let skipped = Str.string_match (Str.regexp {|todo|}) line pos in
    let whitespaceEnd = Str.match_end () in
    if skipped then skipLeadingWhitespace lines whitespaceEnd else ifNone

type position = {
  lineNumber : int;
  remainingLines : string;
  posInLine : int;
}

(*
This function parses a string according to a list of rules. It returns the AST if the parse was successful.  
The output tree contains labels, and a pair of integers which are the positions in the input string corresponding to that node.
*)
let rec parse (compare : 'sort -> 'sort -> bool) (lang : ('sort, 'label) language)
  (remainingLinesBeforeSpace : string list) (posInLineBeforeSpace : int) (where : ('sort, 'label) path) : ('label ast) option =
  let (remainingLines, posInLine) = skipLeadingWhitespace remainingLinesBeforeSpace posInLineBeforeSpace in
  let findRule (sort : 'sort) (above : ('sort, 'label) path) =
      List.find_map
        (fun (Rule(newLabel, newSort, newPattern)) ->
          if not (compare newSort sort) || (amILooping above newLabel) then None else
            (parse compare lang remainingLines posInLine (PNode (above, newLabel, [], newPattern))) 
        )
      lang
  in
  match where with
  | Top sort -> findRule sort (Top sort)
  | PNode (above, label, children, []) ->
      let tree = Node(AstNode label, children) in
      (match above with
        | PNode (above, l, ts, patterns) -> parse compare lang remainingLines posInLine (PNode (above, l, (ts @ [tree]), patterns))
        | Top _sort -> if List.tl remainingLines = [] && posInLine = String.length (List.hd remainingLines) then Some tree else None)
  | PNode (above, label, leftChildren, patterns) -> (
    match patterns with
    | SortPattern sort :: patterns' -> findRule sort (PNode (above, label, leftChildren, patterns'))
    | Keyword expected :: patterns' ->
      if remainingLines = [] then None else
      if expected = String.sub (List.hd remainingLines) posInLine (String.length expected)
        then
          let end_position = (posInLine + String.length expected) in
          parse compare lang remainingLines end_position (PNode(above, label, leftChildren (*@ [String actual]*), patterns'))
        else None
    | RegPattern expected :: patterns' ->
      if remainingLines = [] then None else
      if Str.string_match expected (List.hd remainingLines) posInLine
        then
          let end_position = Str.match_end () in
          parse compare lang remainingLines end_position
            (PNode(above, label, leftChildren @ [Node(AstString (matched_string (List.hd remainingLines)), [])], patterns'))
        else None
    | _ -> None
  )

(*
 Note to self:
 This version has a few improvements over the version in the other file:
 1) [x] - It works generically over sorts, labels, and a comparison function. This should make it easier to integrate with other things
 2) [x] - It doesn't assume that the string should be tokenized by whitespace (it nevertheless assumes that whitespace can be skipped)
  I just had a realization - I can make a rule "whitespace : Regex (matches whitespace) any -> any".
  This is equivalent to the "skipWhitespace" function that I wrote!
 3) [ ] - I also need to make it keep track of the string positions in the tree.
 4) [x] - I also need to make it automatically skip whitespace.
  I just had a realization - I can make a rule "whitespace : Regex (matches whitespace) any -> any".
  This is equivalent to the "skipWhitespace" function that I wrote!
 5) [x] - I also need to make it input a list of lines rather than just a single string.
 6) [ ] - I also need to make it capable of giving some kind of error message with position when it fails.
        - Maybe keep track of how far it gets in input during backtracking, and assume that the first bit of string that it doesn't ever
          successfully match with anything is the problem?
*)