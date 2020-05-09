(* This program is a parser for friendship bracelet patterns. *)
module ProgramParser
open Parser

(* Backus-Naur Grammar for Friendship Bracelet Pattern

<pattern> ::= <comp>+
   <comp> ::= <block>
           |  (repeat num <comp>+)
  <block> ::= <row>+
    <row> ::= <knot>+
   <knot> ::= RR | LL | RL | LR | SKIP
    <num> ::= n in Z+ (aka. any positive integer)

*)

(* AST *) 
type Knot = // four possible knots: user will type ABCD for simplicity 
| RR of char // "A"
| LL of char // "B"
| RL of char // "C"
| LR of char // "D"
| SKIP of char // "_" denotes empty knot (aka. skip string position)
type Row =  
| Row of Knot list
type Component = // pattern has 1+ rows
| Block of Row list
| Repeat of int * Component list // (n, repeated block/pattern)
type Pattern =
| Pattern of Component list

(* PARSER HELPERS *)
// removes parens and returns whatever p returns
let inParens p = pbetween (pchar '(') (pchar ')') p <!> "inParens"
// read in repeat command --> ultimately ignores the results from this parser combinator
let inRepeat p = inParens (pright (pstr "repeat ") p) <!> "inRepeat"

(* GRAMMAR *)
let rr = pchar 'A' |>> RR <!> "rr"
let ll = pchar 'B' |>> LL <!> "ll"
let rl = pchar 'C' |>> RL <!> "rl"
let lr = pchar 'D' |>> LR <!> "lr"
let skip = pchar '_' |>> SKIP <!> "skip"

let comp, compImpl = recparser()
let knot = rr <|> ll <|> rl <|> lr <|> skip <!> "knot"
let row = pmany1 knot |>> Row <!> "row" // knots are assumed to be in order for now
let block = pmany1 (pleft row pws0) |>> Block <!> "block"
let repeat = // formatted like: (repeat 3 AAAA)
    inRepeat (pseq (pleft (pdigit |>> (fun c -> int (string c))) pws1) (pmany1 (pleft comp pws0)) (fun (n, e) -> (n, e)))
    |>> Repeat <!> "repeat"
let pattern = pmany1 (pleft comp pws0) |>> Pattern <!> "expr"
compImpl := block <|> repeat <!> "pattern"
let grammar = pleft pattern peof <!> "grammar"

let parse(s: string) : Pattern option =
    match grammar (prepare s) with
    | Success(res, _) -> Some res
    | Failure _ -> None