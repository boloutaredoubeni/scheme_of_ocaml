open Core.Std

(** Lisp types *)
type t =
  | Atom of string
  (* (string <atom>) *)
  | String of string
  (* true false *)
  | Bool of bool
  (* 1 2 .. 10.0*)
  | Number of float
  (* (x1 x2 ... xn) *)
  | List of t list
  (* (lambda (arg1 .. argn) <expr>) *)
  | Lambda of t list * t
  | Eq of t list
  | Neq of t list
  | Cond of (t * t) list
  | First of t
  | Rest of t
  | Quote of t
  | Add of t list
  | Sub of t list
  | Div of t list
  | Mul of t list
  | Lt of t list
  | Gt of t list
  | Lte of t list
  | Gte of t list
  | And of t list
  | Or of t list
  (* | Not of t *)
  | Cons of t * t
  [@@deriving sexp, eq, show]
