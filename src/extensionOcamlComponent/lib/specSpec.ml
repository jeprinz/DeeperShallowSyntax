open TypeSystem
open Unification

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
let consSort (name : term) (ctx : term) : term = App (App(Const "Cons", name), ctx)
let ctxContainsSort (name : term) (ctx : term) : term = App (App(Const "CtxContains", name), ctx)

let spec : inductive = [
  makeRule (fun var -> 
    {
      name = "LambdaDefCons";
      look = [NameHole; NameKeyword "="; NameHole; NameHole];
      premises = [regexSort (var "name") "[A-Za-z]+"; termSort (consSort (var "name") (var "ctx")); topLevel (consSort (var "name") (var "ctx")) (var "ctxFull")];
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
      premises = [regexSort (var "name") "[A-Za-z]+"; termSort (consSort (var "name") (var "ctx"))];
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

  makeRule (fun var -> {
      name = "Var";
      look = [NameHole];
      premises = [regexSort (var "name") "[A-Za-z]"];
      hiddenPremises = [ctxContainsSort (var "name") (var "ctx")];
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
      conclusion = ctxContainsSort (var "name") (consSort (var "name") (var "ctx"));
      equalities = [];
      disequalities = [var "name1", var "name2"];
  });
]

(*

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