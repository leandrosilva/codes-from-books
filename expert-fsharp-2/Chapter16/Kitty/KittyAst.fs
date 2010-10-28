module Ast

type expr =
    | Val   of string
    | Int   of int
    | Plus  of expr * expr
    | Minus of expr * expr
    | Times of expr * expr

type stmt =
    | Assign     of string * expr
    | While      of expr * stmt
    | Seq        of stmt list
    | IfThen     of expr * stmt
    | IfThenElse of expr * stmt * stmt
    | Print      of expr

type prog = Prog of stmt list

