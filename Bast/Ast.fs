namespace Bast

open Farkle

type Location = Position * Position

type Expr =
    | Char of char
    | String of string
    | Bool of bool
    | Integer of int
    | Symbol of string
    | Unit

type Token = Expr * Location

type SExpr =
    | Atom of Token
    | SList of SExpr list
