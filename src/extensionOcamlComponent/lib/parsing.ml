(* open Js_of_ocaml *)
open Str
open Util

(* Throughout this codebase, we work with generic tree *)
type 'label tree = Node of 'label * ('label tree list)

(*Position within a text file*)
type position = {
  lineNumber : int;
  posInLine : int;
}

type spanPosition = {
  left : position; right : position;
} (* TODO: put spanPosition in some places, like ast. *)

let lessThanPos (pos1 : position) (pos2 : position) : bool =
  (pos1.lineNumber < pos2.lineNumber) || (pos1.lineNumber = pos2.lineNumber && pos1.posInLine < pos2.posInLine)

type 'label astLabel = AstString of string | AstNode of 'label
(* An abstract syntax tree is a tree where each node is either an AstString with no children, or
   a Node with labels from a set.
   Each node also has the beginning and ending position of the node in the text file.*)
type 'label ast = ('label astLabel * position * position) tree


type 'sort pattern =
  Keyword of string
  | NewlinePattern
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

let show_ast_label_short ((l, _p1, _p2) : (string astLabel * position * position)) : string =
  let lString = match l with
    | AstString s -> s
    | AstNode l -> l
  in
  lString

let show_ast_label_short_2 (show_label : 'label -> string) ((l, _p1, _p2) : ('label astLabel * position * position)) : string =
  let lString = match l with
    | AstString s -> s
    | AstNode l -> show_label l
  in
  lString

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
Each node in the path also has the position in the text file for the left end of the node.
*)
type ('sort, 'label) path =
  PNode of ('sort, 'label) path * 'label * position * ('label ast list) * ('sort pattern list)
  | Top of 'sort

let show_top_of_path (show_label : 'label -> string) (p : ('sort, 'label) path) : string =
  match p with
  | Top _ -> "TOP"
  | PNode (_above, label, _pos, _matchedSoFar, _patternsLeft) -> "Where: " ^ show_label label

(* To prevent infinite recursion on left-recursive rules, this function checks if any input has been accepted since the last instance of a rule. *)
let rec amILooping (path : ('sort, 'label) path) (ctr : 'label) (pos : position) : bool =
  match path with
  | PNode (_, _, pos', _, _) when pos' <> pos -> false
  | PNode (above, label, _, _, _) ->
    label = ctr || (amILooping above ctr pos)
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

let firstWhitespace (s : string) (starting_pos : int) : int option =
  try
    Some (Str.search_forward (Str.regexp "[ \n\r\x0c\t]") s starting_pos)
  with
    Not_found -> None

(*
This function parses a string according to a list of rules. It returns the AST if the parse was successful.  
The output tree contains labels, and a pair of integers which are the positions in the input string corresponding to that node.
*)
let parse (compare : 'sort -> 'sort -> bool) (lang : ('sort, 'label) language)
  (show_rule : 'label -> string)
  (show_sort : 'sort -> string)
  (input : string list) (pos : position) (where : ('sort, 'label) path) : ('label ast, (string * position)) result =
  (* Firebug.console##log ("Here2.2"); *)
  (* In order to keep track of errors, whenever it backtracks it remembers the furthest position it got to. We assume this is where the error is. *)
  let furthestPos : position ref = ref {lineNumber = 0; posInLine = 0} in
  let errorMessage : string ref = ref "" in
  let rec parseImpl (remainingLinesBeforeSpace : string list) (posBeforeSpace : position) (where : ('sort, 'label) path) : ('label ast) option =
    (* Firebug.console##log ("parseImpl called at pos: " ^ show_position posBeforeSpace ^ (if (List.length remainingLinesBeforeSpace <> 0) then "And rest of line is: " ^ (List.hd remainingLinesBeforeSpace) else "")); *)
    (* print_endline ("parseImpl called at pos: " ^ show_position posBeforeSpace ^ (if (List.length remainingLinesBeforeSpace <> 0) then "And rest of line is: " ^ (List.hd remainingLinesBeforeSpace) else "")); *)
    (* print_endline ("parseImpl called at pos: " ^ show_position posBeforeSpace ^ "At: " ^ show_top_of_path show_rule where); *)
    let (remainingLines, pos) = skipLeadingWhitespace remainingLinesBeforeSpace posBeforeSpace in
    let newPossibleError (msg : string) =
      if lessThanPos !furthestPos pos && not (!furthestPos = pos) then
        (furthestPos := pos; errorMessage := msg)
      else ()
    in
    let findRule (sort : 'sort) (above : ('sort, 'label) path) =
        List.find_map
          (fun (Rule(newLabel, newSort, newPattern)) ->
            if not (compare newSort sort) || (amILooping above newLabel pos) then
              (newPossibleError ("No rule matches with sort " ^ show_sort sort) ; None)
            else
              (parseImpl remainingLines pos (PNode (above, newLabel, pos, [], newPattern))) 
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
          | PNode (above, l, lPos, ts, patterns) -> parseImpl remainingLines pos (PNode (above, l, lPos, (ts @ [tree]), patterns))
          | Top _sort -> if remainingLines = [] then Some tree else (newPossibleError "Expected end of file" ; None))
    (* If there are more patterns, then handle the next pattern *)
    | PNode (above, label, lPos, leftChildren, patterns) -> (
      match patterns with
      | SortPattern sort :: patterns' -> findRule sort (PNode (above, label, lPos, leftChildren, patterns'))
      | Keyword expected :: patterns' ->
        if remainingLines = [] then (newPossibleError ("Expected " ^ expected ^ " and " ^ string_of_int (List.length patterns') ^ " patterns left") ; None) else
        if String.length (List.hd remainingLines) >= pos.posInLine + (String.length expected) &&
          expected = String.sub (List.hd remainingLines) pos.posInLine (String.length expected)
          then
            let end_position = (pos.posInLine + String.length expected) in
            parseImpl remainingLines {pos with posInLine = end_position} (PNode(above, label, lPos, leftChildren (*@ [String actual]*), patterns'))
          else (newPossibleError ("Expected " ^ expected) ; None)
      | RegPattern expected :: patterns' ->
        if remainingLines = [] then (newPossibleError "Extra stuff" ; None) else
        if Str.string_match expected (List.hd remainingLines) pos.posInLine
          then
            let match_end = Str.match_end () in
            (* Never allows a regex to match more than one word in between whitespace *)
            let endOfMatch = match firstWhitespace (List.hd remainingLines) pos.posInLine with Some ws_pos -> min ws_pos match_end  | None -> match_end in
            let matched_string = String.sub (List.hd remainingLines) pos.posInLine (endOfMatch - pos.posInLine) in
            let end_position = {pos with posInLine = endOfMatch} in
            parseImpl remainingLines end_position
              (PNode(above, label, lPos, leftChildren @ [Node((AstString matched_string, pos, end_position), [])], patterns'))
          else (newPossibleError ("Expected token matching regex") ; None)
      | NewlinePattern :: patterns' ->
        (* TODO: The NewlinePattern doesn't really seem to work. I should either remove it or fix it. *)
        if posBeforeSpace.lineNumber = pos.lineNumber then ( None)
        else parseImpl remainingLines pos (PNode(above, label, lPos, leftChildren, patterns'))
      | _ -> (newPossibleError "Had no more patterns to match" ; None)
    )
  in match parseImpl input pos where with
     | Some t -> Ok t
     | None -> Error (!errorMessage, !furthestPos)

(* In order to deal with left-recursive rules in the grammar, these functions translate the grammar into an alternate form which
   is equivalent but not left-recursive. The idea is to convert left-recursive structure into right-recursive lists.

   This transformation is described in a comment at the bottom of the file.
*)

type 'sort internalSort = NormalSort of 'sort | PostFixSort of 'sort | AtomSort of 'sort
type 'label internalLabel = NormalLabel of 'label | ConsLabel of 'label | NilLabel | OfListLabel

let show_internalLabel (show_label : 'label -> string) (l : 'label internalLabel) : string =
  match l with
  | NormalLabel l -> show_label l
  | ConsLabel l -> "Cons " ^ show_label l
  | NilLabel -> "Nil"
  | OfListLabel -> "OfList"

let show_internalSort (show_sort : 'sort -> string) : 'sort internalSort -> string =
  fun s ->
  match s with
  | NormalSort s -> show_sort s
  | PostFixSort s -> "PostFix: " ^ show_sort s
  | AtomSort s -> "Atom: " ^ show_sort s

(* TODO: Simplify this if the stuff is no longer necessary *)
let rewriteInputPattern (compare : 'sort -> 'sort -> bool) (leftRecursiveSorts : 'sort list) (p : 'sort pattern) : 'sort internalSort pattern =
  match p with
  | Keyword s -> Keyword s
  | NewlinePattern -> NewlinePattern
  | RegPattern r -> RegPattern r
  | SortPattern s ->
    if (Option.is_some (List.find_opt (compare s) leftRecursiveSorts))
      then SortPattern (NormalSort s)
      else SortPattern (NormalSort s)

let rewriteConsInputPattern (isRecursiveSort : 'sort -> bool) (p : 'sort pattern) : 'sort internalSort pattern =
  match p with
  | Keyword s -> Keyword s
  | NewlinePattern -> NewlinePattern
  | RegPattern r -> RegPattern r
  | SortPattern s ->
    if (isRecursiveSort s)
      then SortPattern (AtomSort s)
      else SortPattern (NormalSort s)

(* the two special rules for each sort *)
let nilRule (s : 'sort) : ('sort internalSort, 'label internalLabel) rule =
  Rule (NilLabel, PostFixSort s, [])

let ofListRule (s : 'sort) : ('sort internalSort, 'label internalLabel) rule  =
  Rule(OfListLabel, NormalSort s, [SortPattern (AtomSort s); SortPattern (PostFixSort s)])

let rewriteRules (compare : 'sort -> 'sort -> bool) (rules : ('sort, 'label) language) : ('sort internalSort, 'label internalLabel) language =
  (* TODO: This will have duplicates potentially for sorts used in left recursion multiple times... *)
  let sortsUsedInLeftRecursion = List.filter_map (
    fun (Rule(_label, sort, patterns)) -> match patterns with
      | SortPattern p :: _rest when compare p sort -> Some sort
      | _ -> None
  ) rules in
  let convertedRules = List.map (
    fun (Rule(label, sort, patterns)) -> match patterns with
      (* left recursive rule *)
      | SortPattern p :: rest when compare p sort ->
        Rule(ConsLabel label, PostFixSort sort, (List.map (rewriteConsInputPattern (compare sort)) rest) @ [SortPattern (PostFixSort sort)])
      (* regular rule *)
      | _ -> Rule(NormalLabel label,
          (if (Option.is_some (List.find_opt (compare sort) sortsUsedInLeftRecursion)) then AtomSort sort else NormalSort sort),
          List.map (rewriteInputPattern compare sortsUsedInLeftRecursion) patterns)
  ) rules in
  convertedRules @ (List.map nilRule sortsUsedInLeftRecursion) @ (List.map ofListRule sortsUsedInLeftRecursion)

let rewriteCompare (compare : 'sort -> 'sort -> bool) : ('sort internalSort -> 'sort internalSort -> bool) =
  fun s1 s2 -> match s1, s2 with
  | NormalSort s1', NormalSort s2' -> compare s1' s2'
  | PostFixSort s1', PostFixSort s2' -> compare s1' s2'
  | AtomSort s1', AtomSort s2' -> compare s1' s2'
  | _ -> false

exception Error of string

(*And these functions convert the resulting transformed ast back into an ast for the original grammar*)
let rec convertBack (t : 'label internalLabel ast) : 'label ast =
  match t with
  | Node ((AstNode OfListLabel, _leftPos, _rightPos), [inside; list]) -> unravelList (convertBack inside) list
  | Node ((AstNode NormalLabel l, leftPos, rightPos), children) -> Node((AstNode l, leftPos, rightPos), List.map convertBack children)
  | Node((AstString s, leftPos, rightPos), []) -> Node((AstString s, leftPos, rightPos), [])
  | _ -> raise (Error ("convertBack:" ^ show_tree (fun l -> show_ast_label_short_2 (show_internalLabel (fun x -> x)) l) t))

and unravelList (inside : 'label ast) (t : 'label internalLabel ast) : 'label ast =
  match t with
  | Node ((AstNode NilLabel, _, _), []) -> inside
  | Node ((AstNode ConsLabel s, _leftPos, _rightPos), iChildren) -> (
      (* match (List.reverse iChildren) with
      | ((AstNode OfListLabel, _, _), children) -> _
      | _ -> *)
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

let doParse (lang : ('sort, 'label) language) (show_rule : 'label -> string) (show_sort : 'sort -> string) (lines : string list) (topSort : 'sort) (compare : 'sort -> 'sort -> bool) : ('label ast, string) result =
  let internalRules = rewriteRules compare lang in
  match (parse (rewriteCompare compare) internalRules (show_internalLabel show_rule) (show_internalSort show_sort) lines {lineNumber = 0; posInLine = 0} (Top (NormalSort topSort))) with
  | Ok ast ->
    (* print_endline ("Parsed internal tree: " ^ show_tree (fun l -> show_ast_label_short_2 (show_internalLabel (fun x -> x)) l) ast); *)
    Ok (convertBack ast)
  | Error (msg, pos) -> Error ("At " ^ show_position pos ^ " " ^ msg)

let doParse2 (lang : ('sort, 'label) language) (show_rule : 'label -> string) (show_sort : 'sort -> string) (lines : string list) (topSort : 'sort) (compare : 'sort -> 'sort -> bool) : ('label ast, position * string) result =
  let internalRules = rewriteRules compare lang in
  (* Firebug.console##log ("Here2.1"); *)
  match (parse (rewriteCompare compare) internalRules (show_internalLabel show_rule) (show_internalSort show_sort) lines {lineNumber = 0; posInLine = 0} (Top (NormalSort topSort))) with
  | Ok ast -> Ok (convertBack ast)
  | Error (msg, pos) -> Error (pos, msg)

(*
   
   Description of the left-recursive rule transformation:
   Given a rule of the form
   
   s1 s2 ... sn
   ----------- r
   c
   
   where s1 matches with c, this rule is left recursive.
   We then create a new sort (NormalSort s1), and create new rules:

   (AtomSort s1) s2 ... sn                          (where is sn matches with s1, then make it (AtomSort sn) instead)
   -------------------------- ConsLabel r
   PostFixSort s1

  -------------- NilLabel
  (PostFixSort s1)

  and for any non left-recursive output premise matching with s1, convert it to (AtomSort s1).

  So, for example, the language

  Term "+" Term
  ---------------- plus
  Term

  "f" Term
  -------------
  Term

  "bla"
  ----------------
  SomethingElse Term

  Is transformed into


  "+" (NormalSort Term) (PostFixSort Term)
  ------------------------------------------- ConsLabel plus
  PostFixSort Term

  "f" (NormalSort Term)
  --------------------
  AtomSort Term

  "bla"
  ----------------
  SomethingElse Term                <---------- This doesn't need to get rewritten recursively. That also doesn't make sense given the way this all works.

  And then we have

  ---------------- Nil
  PostFixSort Term

  (AtomSort Term) (PostFixSort Term)
  ---------------------------------- OfListLabel Term
  NormalSort Term
   *)

(*
 Current issue: We end up with
 (cons "+" (OfList 5 (cons "+" (OfList 5 Nil))) Nil)
 Instead of
 (cons "+" (OfList 5 Nil) (cons "+" (OfList 5 Nil) Nil)))

 So, maybe inputs to any Cons should get rewritten to be atoms?


 ANOTHER IDEA:

 Maybe OfListLabel and Cons should actually be the same thing?
 ^^^^^ This seems right.
*)