(* open Str;; *)

open Language;;


let matchElem (se : stackElem) (st : pattern) (keywords : StringSet.t) : tree option =
    match se, st with
    | StringElem actual, Keyword expected -> if actual = expected then Some (String actual) else None
    | StringElem actual, RegPattern expected ->
        if not (StringSet.mem actual keywords) && sane_regex_match expected actual then Some (String actual) else None
    | Tree (actual, t), SortPattern expected -> if actual = expected then Some t else None
    | _ -> None

(*This lists should be in reverse order from what is written in the Rules
   If it succeeds in a match, it returns the popped stack and the popped children*)
let matchLists (stack : stackElem list) (reverseChildren : pattern list) (keywords : StringSet.t) : (stackElem list * tree list) option =
  let rec impl stack reverseChildren (children : tree list)
    = match stack, reverseChildren with
      | _ , [] -> Some (stack, children)
      | s :: stack', child1 :: reverseChildren' ->
          Option.bind (matchElem s child1 keywords) (fun tr ->
            impl stack' reverseChildren' (tr :: children))
      | _, _ -> None
  in impl stack reverseChildren []

type language = rule list
type preProcessedLanguage = {
  reversedRules : rule list;
  keywords : StringSet.t
} 

let preProcessLanguage (lang : language) : preProcessedLanguage =
  let isKeyword (p : pattern) : string option =
    match p with
    | Keyword s -> Some s
    | _ -> None
  in
  {
    reversedRules = List.map (fun (Rule (l, c, hs)) -> Rule (l, c, List.rev hs)) lang;
    keywords = StringSet.of_list (List.concat (List.map (fun (Rule (_label, _sort, hs)) -> List.filter_map isKeyword hs) lang));
  }

(* type parseState = {
  stack : stackElem list;
  position : int;
} *)

(* type parseResult =
  InProcess of parseState
  | Success of tree
  | Failure *)

let tokenize (input : string) : string list =
  (* https://stackoverflow.com/questions/39813584/how-to-split-on-whitespaces-in-ocaml *)
  Str.split (Str.regexp "[ \n\r\x0c\t]+") input

let show_stack_elem (se : stackElem) : string =
  match se with
  | StringElem s -> s
  | Tree (s, t) -> show_tree t ^ " : " ^ s

let show_list (show : 'a -> string) (l : 'a list) : string =
  "[" ^ String.concat ", " (List.map show l) ^ "]"

let parse (input : string) (lang : language) : (tree, string) result =
  let {reversedRules; keywords} = preProcessLanguage lang in
  let rec impl (tokens : string list) (stack : stackElem list) : (tree, string) result =
    let matched = List.find_map (fun (Rule (label, sort, pattern)) ->
        Option.bind (matchLists stack pattern keywords) (fun (newStack, children) ->
          Some (Tree (sort, Node(label, children)) :: newStack ))) reversedRules 
    in
    match matched with
    (* If there is a rule which matches the top of the stack *)
    | Some newStack -> impl tokens newStack
    (* If no rule matches the top of the stack, pop a token *)
    | None -> match tokens with
      | token :: tokens' -> print_endline ("processing token: " ^ token) ;impl tokens' (StringElem token :: stack)
      | [] -> match stack with (* If no more tokens, we are done *)
        | Tree (_, t) :: [] -> Ok t (*If exactly one tree is left on the stack, parse was successful*)
        | _ -> Error ("Error: Remaining stack: " ^ show_list show_stack_elem stack) (*In any other situation, the parse failed*)
  in impl (tokenize input) [] 