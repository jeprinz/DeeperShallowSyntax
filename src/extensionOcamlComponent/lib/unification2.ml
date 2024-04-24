type id = int
  [@@deriving show]
let nextId : int ref = ref 0
let freshId (_ : unit) : id =
  let next = !nextId in
  nextId := !nextId + 1
  ; next

type label = 
  MetaVar of id
  | Lam (*1 child*)
  | App (*2 childred*)
  | Var of int (*0 children*)
  | Const of string (*0 children*)
  | StringLiteral of string (*0 children*)
  | MetaData of string (*1 child*)

type term =
  MetaVar of id
  | Lam of term
  | App of term * term
  | Var of int
  | Const of string
  | MetaData of string * term
  (* [@@deriving show] *)