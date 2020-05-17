(* This program is a parser for friendship bracelet patterns. *)
module ProgramParser
open Parser

(* Backus-Naur Grammar for Friendship Bracelet Pattern

<pattern> ::= <name> <strings> <comp>+
   <name> ::= string
<strings> ::= num <string>+
   <comp> ::= <block>
           | (repeat num <comp>+)
 <string> ::= any valid alphanumeric string
  <block> ::= <row>+
    <row> ::= <knot>+
   <knot> ::= RR | LL | RL | LR | SKIP
    <num> ::= n in Z+ (aka. any positive integer)

Note: colors should be specified by html color name: https://www.w3schools.com/colors/colors_names.asp
      or html color code, ex. #fff.

*)

(* AST *) 
type Knot = // four possible knots: user will type ABCD for simplicity 
| RR of char // "A"
| LL of char // "B"
| RL of char // "C"
| LR of char // "D"
| SKIP of char // "_" denotes empty knot (aka. skip string position)
type Row =  
| Row of Knot list // row has 1+ knots
type Component = 
| Block of Row list // pattern has 1+ rows
| Repeat of int * Component list // (n, repeated block/pattern)
type Name =
| Name of string // name of pattern
type Strings =
| Strings of int * string list // list of strings + colors in their starting order
type Pattern =
| Pattern of Name * Strings * Component list // pattern must have a name, list of strings, and 1+ components

(* PARSER HELPERS *)
// removes parens and returns whatever p returns
let inParens p = pbetween (pchar '(') (pchar ')') p <!> "inParens"
// read in repeat command --> ultimately ignores the results from this parser combinator
let inRepeat p = inParens (pright (pstr "repeat ") p) <!> "inRepeat"
// read in strings command --> ultimately ignores the results from this parser combinator
let stringHelper p = inParens (pright (pstr "strings ") p) <!> "stringHelper"
// read in name command --> ultimately ignores the results from this parser combinator
let nameHelper p = inParens (pright (pstr "name ") p) <!> "nameHelper"
// read a word as a series of characters and turn it into a string
let word = pmany1 (pletter <|> pdigit <|> pchar '#') |>> stringify <!> "word"
// parse a number and a list of 'p'
let numAndList p = pseq (pleft (pdigit |>> (fun c -> int (string c))) pws1) (pmany1 (pleft p pws0)) (fun (n, e) -> (n, e)) <!> "numAndList"
// parses a white space or a new line
let wsOrNl p = pleft p ((pws0 |>> stringify) <|> pnl) <!> "wsOrNl"

(* GRAMMAR *)
// knots
let rr = pchar 'A' |>> RR <!> "rr"
let ll = pchar 'B' |>> LL <!> "ll"
let rl = pchar 'C' |>> RL <!> "rl"
let lr = pchar 'D' |>> LR <!> "lr"
let skip = pchar '_' |>> SKIP <!> "skip"

let comp, compImpl = recparser()
// components
let knot = rr <|> ll <|> rl <|> lr <|> skip <!> "knot"
let row = pmany1 knot |>> Row <!> "row" // knots are assumed to be in order
let block = pmany1 (wsOrNl row) |>> Block <!> "block"

// operations
let name = nameHelper word |>> Name <!> "name" // formatted like: (name MYPATTERN)
let strings = stringHelper (numAndList word) |>> Strings <!> "strings" // formatted like: (strings 3 red blue purple)
let repeat = inRepeat (numAndList comp) |>> Repeat <!> "repeat" // formatted like: (repeat 3 AAAA)

// big picture stuff
let patternHelper = pmany1 (wsOrNl comp) <!> "pattern helper"
let pattern = pseq3 (wsOrNl name) (wsOrNl strings) (wsOrNl patternHelper) (fun (a, b, c) -> (a, b, c)) |>> Pattern <!> "pattern"
compImpl := block <|> repeat <!> "comp"
let grammar = pleft pattern peof <!> "grammar"

// actually parse the expression now :)
let parse(s: string) : Pattern option =
    match grammar (prepare s) with
    | Success(res, _) -> Some res
    | Failure (pos, rule) -> 
        let msg = sprintf "Cannot parse input at pos %d in rule '%s':" pos rule
        let diag = diagnosticMessage 20 pos s msg
        printfn "%s" diag
        None