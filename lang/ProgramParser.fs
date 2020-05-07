(* This program is a parser for friendship bracelet patterns. *)
module ProgramParser
open Parser

(* Backus-Naur Grammar for Friendship Bracelet Pattern

<pattern> ::= <row>+
    <row> ::= <knot>+
   <knot> ::= RR | LL | RL | LR

*)

(* AST *) 
type Knot = // four possible knots: user will type ABCD for simplicity
| RR of char // "A"
| LL of char // "B"
| RL of char // "C"
| LR of char // "D"
type Row =  // row has 1+ knots
| Knots of Knot list
type Pattern = // pattern has 1+ rows
| Block of Row list 
| Repeat of int * Pattern // (n, repeated block/pattern)

(* PARSER HELPERS *)
// form list of parsed items
// let pmany2sep p sep = pseq p (pmany1 (pright sep p)) (fun (x, xs) -> x::xs)

(* GRAMMAR *)
let rr = pchar 'A' |>> RR
let ll = pchar 'B' |>> LL
let rl = pchar 'C' |>> RL
let lr = pchar 'D' |>> LR

let pattern, patternImpl = recparser()
let knot = rr <|> ll <|> rl <|> lr 
let row = (pmany1 knot) |>> Knots // knots are assumed to be in order for now
let block = (pmany1 row) |>> Block
let repeat = 
    (pseq (pright (pright (pstr "repeat") pws1) pdigit) (pbetween (pchar '(') (pchar ')') pattern) (fun (n, e) -> (n, e)))
    |>> Repeat 
patternImpl := block <|> repeat
let grammar = pleft pattern peof

let parse(s: string) : Pattern option =
    match grammar (prepare s) with
    | Success(res, _) -> Some res
    | Failure _ -> None