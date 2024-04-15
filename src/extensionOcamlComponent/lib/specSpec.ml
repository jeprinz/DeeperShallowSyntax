open TypeSystem
open Unification
open Parsing
open Util

(*
In this file, I define a type system for defining type systems.
An example of what a constructor should look like:

example = \a b c. a b

Regex s "[A-Za-z]+"
Term (Cons s gamma a) b 
|------------------------------------- "fun _ => _"
Term gamma (arrow a b)
*)

let termSort (ctx : term) : term = App (Const "Term", ctx)
let topLevel (ctx : term) (fullCtx : term) : term = App (App(Const "TopLevel", ctx), fullCtx)
let consSort (name : term) (ctx : term) : term = App (App(Const "CtxCons", name), ctx)
let nilSort : term = Const "CtxNil"
let ctxContainsSort (name : term) (ctx : term) : term = App (App(Const "CtxContains", name), ctx)
let premiseList (ctx : term) : term = App (Const "PremiseList", ctx)
let premise (ctx : term) : term = App (Const "Premise", ctx)
let nameComponentSort  : term = Const "NameComponent"
let nameComponentListSort : term = Const "NameComponentList"
let varListSort (ctxOutside : term) (ctxInside : term)  : term = App(App(Const "VarList", ctxOutside), ctxInside)

let regexAstToString (t : string ast) : string =
  match t with
  | Node((AstString str, _, _), []) -> str
  | _ -> raise (Error "Term ast wasn't of correct form")

(*
I need to deal with that variables can be either locally bound, or global definitions. But the global definitions can be mutually  
referential.

If I want to use my existing implementation of lambda calculus terms, my only choice is to represent global definitions as
metavariables, and also give an environment.
*)

let getMV (globalMvNames : id StringMap.t ref) (name : string) =
  if StringMap.mem name !globalMvNames
    then ()
    else globalMvNames := StringMap.add name (freshId ()) !globalMvNames;
  StringMap.find name !globalMvNames

let rec termAstToTerm (mvEnv : id StringMap.t ref) (globalMvNames : id StringMap.t ref) (localEnv : string list) (t : string ast) : term =
  match t with
  | Node((AstNode "Lambda", _, _), [Node((AstNode "VarListNil", _, _), []); body]) -> termAstToTerm mvEnv globalMvNames localEnv body
  | Node((AstNode "Lambda", p1, p2), [Node((AstNode "VarListCons", _, _), [nameRegex; rest]); body]) ->
    Lam  (termAstToTerm mvEnv globalMvNames (regexAstToString nameRegex :: localEnv) (Node((AstNode "Lambda", p1, p2), [rest; body])))
  | Node((AstNode "Parens", _, _), [t]) -> termAstToTerm mvEnv globalMvNames localEnv t
  | Node((AstNode "Application", _, _), [t1; t2]) -> App (termAstToTerm mvEnv globalMvNames localEnv t1, termAstToTerm mvEnv globalMvNames localEnv t2)
  | Node((AstNode "Var", _, _), [nameRegex]) ->
      let name = (regexAstToString nameRegex) in
      (match (Util.find_index localEnv name) with
      | Some n -> Var n
      | None -> MetaVar (getMV globalMvNames name))
  | Node((AstNode "Const", _, _), [nameRegex]) -> Const (regexAstToString nameRegex)
  | Node((AstNode "MetaVar", _, _), [nameRegex]) -> MetaVar (getMV mvEnv (regexAstToString nameRegex))
  | _ -> raise (Error "Term ast wasn't of correct form")

type premise = TermPremise of term | EqualityPremise of (term * term) | DisequalityPremise of (term * term) | HiddenPremise of term

let premiseAstToPremise (mvEnv : id StringMap.t ref) (globalMvNames : id StringMap.t ref) (t : string ast) : premise =
  match t with
  | Node((AstNode "TermPremise", _, _), [term]) -> TermPremise (termAstToTerm mvEnv globalMvNames [] term)
  | Node((AstNode "EqualityPremise", _, _), [t1; t2]) -> EqualityPremise(termAstToTerm mvEnv globalMvNames [] t1, termAstToTerm mvEnv globalMvNames [] t2)
  | Node((AstNode "DisequalityPremise", _, _), [t1; t2]) -> DisequalityPremise(termAstToTerm mvEnv globalMvNames [] t1, termAstToTerm mvEnv globalMvNames [] t2)
  | Node((AstNode "HiddenPremise", _, _), [term]) -> HiddenPremise (termAstToTerm mvEnv globalMvNames [] term)
  | _ -> raise (Error "Premise list ast wasn't of correct form")

let rec premiseListAstToPremises (mvEnv : id StringMap.t ref) (globalMvNames : id StringMap.t ref) (t : string ast) : premise list =
  match t with
  | Node((AstNode "PremiseListCons", _, _), [premise; premises]) ->
      premiseAstToPremise mvEnv globalMvNames premise :: premiseListAstToPremises mvEnv globalMvNames premises
  | Node((AstNode "PremiseListLastCons", _, _), [premise]) -> premiseAstToPremise mvEnv globalMvNames premise :: []
  | Node((AstNode "PremiseListNil", _, _), []) -> []
  | _ -> raise (Error "Premise list ast wasn't of correct form")

type partialConstructor = {premises : term list; hiddenPremises : term list; equalities : (term * term) list; disequalities : (term * term) list}
(* outputs premises, equalities, and disequalities *)
let premiseListDecompose (premises : premise list) : partialConstructor =
  List.fold_right (fun premise ctr -> 
    match premise with
    | TermPremise t -> {ctr with premises = t :: ctr.premises}
    | EqualityPremise eq -> {ctr with equalities = eq :: ctr.equalities}
    | DisequalityPremise deq -> {ctr with disequalities = deq :: ctr.disequalities}
    | HiddenPremise t -> {ctr with hiddenPremises = t :: ctr.hiddenPremises}
  ) premises {premises = []; hiddenPremises = []; equalities = []; disequalities = []}

let nameComponentAstToNameComponent (t : string ast) : nameComponent =
  match t with
  | Node((AstNode "NameHole", _, _), []) -> NameHole
  | Node((AstNode "NameKeyword", _, _), [text]) -> NameKeyword (regexAstToString text)
  | _ -> raise (Error "NameComponentList ast wasn't of correct form")

let rec nameComponentListAstToNaming (t : string ast) : naming =
  match t with
  | Node((AstNode "NameComponentListCons", _, _), [name; names]) ->
      nameComponentAstToNameComponent name :: nameComponentListAstToNaming names
  | Node((AstNode "NameComponentListNil", _, _), []) -> []
  | _ -> raise (Error "NameComponentList ast wasn't of correct form")

(* Inputs an AST parsed from the below spec, and outputs it as a language *)
let specAstToLang (t : string ast) : inductive =
  (* As it parses the syntax, it stores global definitions in an enviroment, and keeps a mapping of names to metavariables ids. *)
  let globalMvNames : id StringMap.t ref = ref StringMap.empty in
  let metavarMvNames : id StringMap.t ref = ref StringMap.empty in
  let globalEnv : sub ref = ref IntMap.empty in
  let ruleNumber = ref 0 in
  let rec impl (t : string ast) : constructor list =
    match t with
    | Node((AstNode "LambdaDefCons", _, _), [nameRegex; term; rest]) ->
      let t = termAstToTerm metavarMvNames globalMvNames [] term in
      globalEnv := IntMap.add (getMV globalMvNames (regexAstToString nameRegex)) t !globalEnv;
      impl rest
    | Node((AstNode "TypeRuleCons", _, _), [premises; _barNotation; look; conclusion; rest]) ->
      let partialConstructor = premiseListDecompose(premiseListAstToPremises metavarMvNames globalMvNames premises) in
      let naming = nameComponentListAstToNaming look in
      let name = "Rule" ^ string_of_int !ruleNumber ^ " \"" ^ show_naming naming ^ "\"" in
      ruleNumber := !ruleNumber + 1;
      {
        name = name;
        look = naming;
        premises = partialConstructor.premises;
        hiddenPremises = []; (* TODO *)
        conclusion = termAstToTerm metavarMvNames globalMvNames [] conclusion;
        equalities = partialConstructor.equalities;
        disequalities = partialConstructor.disequalities;
      }
      :: impl rest
    | Node((AstNode "Nil", _, _), []) -> []
    | _ -> raise (Error "Lang ast wasn't of correct form")
  in
  let ctrs = impl t in
  (* This is the lazy inefficient solution. Instead of finding which metavars go to which rule, Im just listing them all as bound for all rules. *)
  let allMetaVars = StringMap.fold (fun _ id acc -> id :: acc) !metavarMvNames [] in
  let lang = List.map (fun ctr -> {constructor = subCtr !globalEnv ctr; boundVars = allMetaVars}) ctrs in
  lang
  

(*
 The specification of the language of specifications  
*)

let spec : inductive = [
  makeRule (fun var -> 
    {
      name = "LambdaDefCons";
      look = [NameHole; NameKeyword "="; NameHole; NameHole];
      premises = [regexSort (var "name") "[A-Za-z]+"; termSort (var "ctxFull"); topLevel (consSort (var "name") (var "ctx")) (var "ctxFull")];
      hiddenPremises = [];
      conclusion = topLevel (var "ctx") (var "ctxFull");
      equalities = [];
      disequalities = [];
    });

  makeRule (fun var ->
    {
      name = "Nil";
      look = [];
      premises = [];
      hiddenPremises = [];
      conclusion = topLevel (var "ctx") (var "ctx");
      equalities = [];
      disequalities = [];
    });

  makeRule (fun var ->
    (* A lambda, like "\x . t"*)
    {
      name = "Lambda";
      look = [NameKeyword "\\"; NameHole; NameKeyword "."; NameHole];
      premises = [varListSort (var "ctxOutside") (var "ctxInside"); termSort (var "ctxInside")];
      hiddenPremises = [];
      conclusion = termSort (var "ctxOutside");
      equalities = [];
      disequalities = [];
    });

  makeRule (fun var -> {
      name = "VarListNil";
      look = [];
      premises = [];
      hiddenPremises = [];
      conclusion = varListSort (var "ctx") (var "ctx");
      equalities = [];
      disequalities = [];
  });

  makeRule (fun var -> {
      name = "VarListCons";
      look = [NameHole; NameHole];
      premises = [regexSort (var "name") "[a-z][A-Za-z0-9]*"; varListSort (var "ctxOutside") (var "ctxInside")];
      hiddenPremises = [];
      conclusion = varListSort (var "ctxOutside") (consSort (var "name") (var "ctxInside"));
      equalities = [];
      disequalities = [];
  });

  makeRule (fun var ->
    {
      name = "Parens";
      look = [NameKeyword "("; NameHole; NameKeyword ")"];
      premises = [termSort (var "ctx")];
      hiddenPremises = [];
      conclusion = termSort (var "ctx");
      equalities = [];
      disequalities = [];
    });

  makeRule (fun var ->
    (* An application, like "t t"*)
    {
      name = "Application";
      look = [NameHole; NameHole];
      premises = [termSort (var "ctx"); termSort (var "ctx")];
      hiddenPremises = [];
      conclusion = termSort (var "ctx");
      equalities = [];
      disequalities = [];
    });

  (* Variables start with a lower case letter *)
  makeRule (fun var -> {
      name = "Var";
      look = [NameHole];
      premises = [regexSort (var "name") "[a-z][A-Za-z0-9]*"];
      hiddenPremises = [ctxContainsSort (var "name") (var "ctx")];
      conclusion = termSort (var "ctx");
      equalities = [];
      disequalities = [];
  });

  (* Constants start with an upper case letter *)
  makeRule (fun var -> {
      name = "Const";
      look = [NameHole];
      premises = [regexSort (var "name") "[A-Z][A-Za-z0-9]*"];
      hiddenPremises = [];
      conclusion = termSort (var "ctx");
      equalities = [];
      disequalities = [];
  });

  (* Metavars start with '?' *)
  makeRule (fun var -> {
      name = "MetaVar";
      look = [NameHole];
      premises = [regexSort (var "name") "\?[A-Za-z0-9]*"];
      hiddenPremises = [];
      conclusion = termSort (var "ctx");
      equalities = [];
      disequalities = [];
  });

  makeRule (fun var -> {
      name = "VarZero";
      look = [];
      premises = [];
      hiddenPremises = [];
      conclusion = ctxContainsSort (var "name") (consSort (var "name") (var "ctx"));
      equalities = [];
      disequalities = [];
  });

  makeRule (fun var -> {
      name = "VarSuc";
      look = [NameHole];
      premises = [ctxContainsSort (var "name1") (var "ctx")];
      hiddenPremises = [];
      conclusion = ctxContainsSort (var "name1") (consSort (var "name2") (var "ctx"));
      equalities = [];
      disequalities = [var "name1", var "name2"];
  });

  makeRule (fun var -> {
      name = "MetaHole";
      look = [NameKeyword "???"];
      premises = [];
      hiddenPremises = [App(Const "MetaHoleWas:", var "anything")];
      conclusion = (termSort (var "anything"));
      equalities = [];
      disequalities = [];
  });

  makeRule (fun var -> {
      name = "TermPremise";
      look = [NameHole];
      premises = [termSort (var "ctx")];
      hiddenPremises = [];
      conclusion = (premise (var "ctx"));
      equalities = [];
      disequalities = [];
  });

  makeRule (fun var -> {
      name = "HiddenPremise";
      look = [NameKeyword "{"; NameHole; NameKeyword "}"];
      premises = [termSort (var "ctx")];
      hiddenPremises = [];
      conclusion = (premise (var "ctx"));
      equalities = [];
      disequalities = [];
  });

  makeRule (fun var -> {
      name = "EqualityPremise";
      look = [NameHole; NameKeyword "=="; NameHole];
      premises = [termSort (var "ctx"); termSort (var "ctx")];
      hiddenPremises = [];
      conclusion = (premise (var "ctx"));
      equalities = [];
      disequalities = [];
  });

  makeRule (fun var -> {
      name = "DisequalityPremise";
      look = [NameHole; NameKeyword "!="; NameHole];
      premises = [termSort (var "ctx"); termSort (var "ctx")];
      hiddenPremises = [];
      conclusion = (premise (var "ctx"));
      equalities = [];
      disequalities = [];
  });

  makeRule (fun var -> {
      name = "PremiseListCons";
      look = [NameHole; NameKeyword ","; NameHole];
      premises = [premise (var "ctx"); premiseList (var "ctx")];
      hiddenPremises = [];
      conclusion = (premiseList (var "ctx"));
      equalities = [];
      disequalities = [];
  });

  makeRule (fun var -> {
      name = "PremiseListLastCons";
      look = [NameHole];
      premises = [premise (var "ctx")];
      hiddenPremises = [];
      conclusion = (premiseList (var "ctx"));
      equalities = [];
      disequalities = [];
  });

  makeRule (fun var -> {
      name = "PremiseListNil";
      look = [];
      premises = [];
      hiddenPremises = [];
      conclusion = (premiseList (var "ctx"));
      equalities = [];
      disequalities = [];
  });

  makeRule (fun var -> {
      name = "TypeRuleCons";
      (* look = [NameKeyword "{"; NameHole; NameHole; NameKeyword "\""; NameHole; NameKeyword "\""; NameHole; NameKeyword "}"; NameHole]; *)
      look = [
        NameKeyword "{";
        NameHole; (* premises *)
        NameHole; (* ----------- *)
        NameKeyword "\"";
        NameHole; (* name *)
        NameKeyword "\"";
        NameHole; (* conclusion *)
        NameKeyword "}";
        NameHole];
      premises = [
          premiseList (var "ctx"); (* premises *)
          regexSort (var "_") "-+"; (* ---------------- *)
          (* regexSort (var "name") "\".*\""; (* look *) *)
          nameComponentListSort; (* look *)
          termSort (var "ctx"); (* conclusion *)
          topLevel (var "ctx") (var "ctxFull") (* rest of program *)
        ];
      hiddenPremises = [];
      conclusion = (topLevel (var "ctx") (var "ctxFull"));
      equalities = [];
      disequalities = [];
  });

  makeRule (fun var -> {
      name = "NameKeyword";
      look = [NameHole];
      premises = [regexSort (var "name") "[^\"_]+"]; (* Matches 1 or more characters that are not a quotation mark or _*)
      hiddenPremises = [];
      conclusion = nameComponentSort;
      equalities = [];
      disequalities = [];
  });

  
  makeRule (fun _var -> {
      name = "NameHole";
      look = [NameKeyword "_"];
      premises = [];
      hiddenPremises = [];
      conclusion = nameComponentSort;
      equalities = [];
      disequalities = [];
  });

  makeRule (fun _var -> {
      name = "NameComponentListNil";
      look = [];
      premises = [];
      hiddenPremises = [];
      conclusion = nameComponentListSort;
      equalities = [];
      disequalities = [];
  });

  makeRule (fun _var -> {
      name = "NameComponentListCons";
      look = [NameHole; NameHole];
      premises = [nameComponentSort; nameComponentListSort];
      hiddenPremises = [];
      conclusion = nameComponentListSort;
      equalities = [];
      disequalities = [];
  });

]

(*

Term ctx
TermList ctx
------------------------ "_ _" TermListCons
TermList ctx

------------------------ "" TermListNil
TermList ctx

{
  TermList ctx
  Regex _ "-+"
  Term ctx
  TopLevel ctx ctxFull
-------------------------- "{_ _ _ _}" TypeRuleCons
  TopLevel ctx ctxFull
}

Regex name [name]
{CtxContains name ctx}
----------------------- "_" Var
Term ctx


---------------------------------- "" VarZero
CtxContains name (cons name ctx)


NotEqual name1 name2
CtxContains name1 ctx
------------------------------------ "" VarSuc
CtxContains name1 (cons name2 ctx)


------------------- "" Nil
TopLevel ctx ctx

Regex name [name]
Term (cons name ctx)
TopLevel (cons name ctx) ctxFull
---------------------------------- "_ = _ \n _" LambdaDefCons
TopLevel ctx ctxFull

Regex name [name]
Term (cons name ctx)
------------------------ "\\ _ . _" Lambda
Term ctx

Term ctx
Term ctx
---------------------- "_ _" Application
Term ctx

*)

(* TODO: I may want to add a new pattern to the parser (and a new nameComponent) for newlines.
   This successfully matches if skipWhitespace skipped a nonzero number of lines.*)

(* TODO: I may want to make these rules actually build up the lambda terms themselves at sorts. Well, that doesn't quite
   work fully because I need to deal with metavariables somehow. *)

(*
 PROBLEM: My whole plan doesn't quite work with regards to using a "NotEqual" relation to disallow certian cases.
 This is because the "only one constructor matches" idea doesn't take into account children of that constructor.
 Nor should it, because then the system wouldn't be linear in the size of the eventual built relation.
 
 One possible solution: Make disequality constraints a special thing within rules, which ARE specifically checked when
 hidden arguments are being handled.

 But then, should there be Equality constraints too?
 We sort of implicitly already have that: for example,

 x = f y z
 z = w                                        
 -----------------     can be rewritten as  -----------------------
 Example x y z w                             Example (f y z) y z z

 But, can you do this rewriting in general?
 Surely yes, just do the unification yourself. Unless there are remaining unification constraints?


 But why do I prefer this design overall to another design:
 Don't have hidden arguments whatsoever. Instead, have more functions that get evaluated in the terms,
 including equality and disequality operators which simply get stuck if the inputs are metavariables.

  Honestly, the main motivation is just that this one seems easier to implement in a usable way.

  Consider the linear lambda calculus example. I could have a hidden constraint "OneOf a b c", or I could have a function
  "oneOf : bool -> bool -> bool -> unit", and something like
  oneOf a b c = match a, b, c with
    | f f f -> unit
    | t f t -> unit
    | f t f -> unit
    | _ _ _ -> Error "error message here"
  
  Then, the application constructor could have a constraint "oneOf x y z = unit".

  In this case, its equivalent. But in general, the constraint solving approach allows it to discover one of the three values after the first two
  are known, in any order.

  But, if I were to do it this way by computation in the terms, there would necessarily need to be a match construct.
  This construct could have a set of cases, and cancel them one-by-one, similar to my hidden-argument solving idea.
  When only one possible case remains, it would do the necessary unification.

  But then, there would still need to be disequalities in the match cases...
  
  For that matter, there could be no inductive relation whatsoever.
  The entire thing could just be a program which inputs a tree with labelled nodes, and performs this kind of matching.

  But then, you can't use the same information to make other things, like a structure editor.
*)