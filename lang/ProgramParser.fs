(* This program is a parser for friendship bracelet patterns. *)
module ProgramParser
open Parser

(* Backus-Naur Grammar for Friendship Bracelet Pattern

<pattern> ::= <row>+
    <row> ::= <knot>+
   <knot> ::= RR | LL | RL | LR

*)

(* AST *) 
type Knot = // four possible knots
| LL of string
| RR of string
| RL of string
| LR of string
type Row =  // row has 1+ knots
| Knots of Knot list
type Pattern = // pattern has 1+ rows
| Rows of Row list 

(* PARSER HELPERS *)
// form list of parsed items
let pmany2sep p sep = pseq p (pmany1 (pright sep p)) (fun (x, xs) -> x::xs)

(* GRAMMAR *)
let ll = pstr "LL" |>> LL
let rr = pstr "RR" |>> RR
let rl = pstr "RL" |>> RL
let lr = pstr "LR" |>> LR

// knots are assumed to be in order for now
let knot = ll <|> rr <|> rl <|> lr
let row = (pmany2sep knot pws1) |>> Knots
let expr = (pmany2sep row pnl) |>> Rows
let grammar = pleft expr peof

let parse(s: string) : Pattern option =
    match grammar (prepare s) with
    | Success(res, _) -> Some res
    | Failure _ -> None