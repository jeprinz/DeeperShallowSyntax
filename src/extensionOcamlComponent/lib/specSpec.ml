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

let spec : inductive = [
  (
  let name = freshMetaVar () in
  let ctx = freshMetaVar () in
  let ctxFull = freshMetaVar () in
  (* A lambda term definition, like "true = \x y . x"*)
  {
    name = "LambdaDefCons";
    look = [NameHole; NameKeyword "="; NameHole; NameHole];
    premises = [regexSort name "[A-Za-z]+"; termSort (consSort name ctx); topLevel (consSort name ctx) ctxFull];
    hiddenPremises = [];
    conclusion = topLevel ctx ctxFull;
  });

  (
    let ctx = freshMetaVar () in
    {
      name = "Nil";
      look = [];
      premises = [];
      hiddenPremises = [];
      conclusion = topLevel ctx ctx;
    }
  );

  (
    let name = freshMetaVar () in
    let ctx = freshMetaVar () in
    (* A lambda, like "\x . t"*)
    {
      name = "Lambda";
      look = [NameKeyword "\\"; NameHole; NameKeyword "."; NameHole];
      premises = [regexSort name "[A-Za-z]+"; termSort (consSort name ctx)];
      hiddenPremises = [];
      conclusion = termSort ctx;
    }
  );

  (
    let ctx = freshMetaVar () in
    (* An application, like "t t"*)
    {
      name = "Application";
      look = [NameHole; NameHole];
      premises = [termSort ctx; termSort ctx];
      hiddenPremises = [];
      conclusion = termSort ctx;
    }
  );
]

(*
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

Regex name [name]
{CtxContains name ctx}
----------------------- "_" Var
Term ctx


---------------------------------- VarZero
CtxContains name (cons name ctx)


NotEqual name1 name2
CtxContains name1 ctx
------------------------------------ VarSuc
CtxContains name1 (cons name2 ctx)

*)

(* TODO: I may want to add a new pattern to the parser (and a new nameComponent) for newlines.
   This successfully matches if skipWhitespace skipped a nonzero number of lines.*)

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
*)