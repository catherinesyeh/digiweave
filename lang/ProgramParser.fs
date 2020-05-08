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
type Row =  
| Row of Knot list
type Pattern = // pattern has 1+ rows
| Block of Row list 
| Repeat of int * Pattern list // (n, repeated block/pattern)

(* PARSER HELPERS *)
//let pmany2sep p sep = pseq p (pmany1 (pright sep p)) (fun (x, xs) -> x::xs) <!> "pmany2sep"
// removes parens and returns whatever p returns
let inParens p = pbetween (pchar '(') (pchar ')') p <!> "inParens"
// read in repeat command --> ultimately ignores the results from this parser combinator
let inRepeat p = inParens (pright (pstr "repeat ") p) <!> "inRepeat"

(* GRAMMAR *)
let rr = pchar 'A' |>> RR <!> "rr"
let ll = pchar 'B' |>> LL <!> "ll"
let rl = pchar 'C' |>> RL <!> "rl"
let lr = pchar 'D' |>> LR <!> "lr"

let pattern, patternImpl = recparser()
let knot = rr <|> ll <|> rl <|> lr <!> "knot"
let row = pmany1 knot |>> Row <!> "row" // knots are assumed to be in order for now
let block = pmany1 (pleft row pws0) |>> Block <!> "block"
let repeat = // formatted like: (repeat 3 AAAA)
    inRepeat (pseq (pdigit |>> (fun c -> int (string c))) (pmany1 (pright pws1 pattern)) (fun (n, e) -> (n, e)))
    |>> Repeat <!> "repeat"
//let seq = pmany2sep pattern pws0 |>> Seq <!> "seq"
patternImpl := block <|> repeat <!> "pattern"
let grammar = pleft pattern peof <!> "grammar"

let parse(s: string) : Pattern option =
    match grammar (debug s) with
    | Success(res, _) -> Some res
    | Failure _ -> None