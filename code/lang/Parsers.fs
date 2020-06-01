(*
A simple combinator-style parsing library for F#.

Inspired by the Hutton & Meijer paper as well as the FParsec
combinator library.  Other than being much smaller, this
library trades away performance for simplicity.  If you need
a fast library, look at FParsec.

Version: 1.4 (2019-12-02)
*)

module Parser

open System
open System.Text.RegularExpressions

// input string * pos * debug
type Input = string * int * bool

(* Use this to prepare a rich string for normal parsing operation *)
let prepare(input: string) : Input = input, 0, false

(* Use this to prepare a rich string for grammar debugging *)
let debug(input: string) : Input = input, 0, true

let input i =
  let (e,_,_) = i
  e

let position i =
  let (_,e,_) = i
  e

let isEOF i =
  let pos = position i
  let len = String.length (input i)
  pos >= len

let isDebug i =
  let (_,_,e) = i
  e

type Outcome<'a> =
| Success of result: 'a * remaining: Input
| Failure of fail_pos: int * rule: string

type Parser<'a> = Input -> Outcome<'a>

let recparser() =
    let dumbparser = fun (input: Input) -> failwith "You forgot to initialize your recursive parser."
    let r = ref dumbparser
    (fun (input: Input) -> !r input), r

let cToHex(c: char) = "0x" + System.Convert.ToByte(c).ToString("x2");;

let (<!>)(p: Parser<'a>)(label: string)(i: Input) : Outcome<'a> =
  // if debugging is enabled...
  if (isDebug i) then
      if (input i).Length - (position i) > 0 then
          printfn "[attempting: %s on \"%s\", next char: %s]" label (input i) (cToHex (input i).[0])
      else
          printfn "[attempting: %s on \"%s\", next char: EOF]" label (input i)
      let o = p i
      match o with
      | Success(a, i') ->
          let i1pos = position i
          let i2pos = position i'
          let istr = input i
          let nconsumed = i2pos - i1pos
          let iconsumed = istr.Substring(i1pos, i2pos - i1pos)
          let rem = istr.[i2pos..]
          if istr.Length - i2pos > 0 then
              printfn "[success: %s, consumed: \"%s\", remaining: \"%s\", next char: %s]" label iconsumed rem (cToHex rem.[0])
          else
              printfn "[success: %s, consumed: \"%s\", remaining: \"%s\", next char: EOF]" label iconsumed rem
      | Failure(pos,rule) ->
          let rem = (input i).[pos..]
          if rem.Length > 0 then
              printfn "[failure at pos %d in rule %s: %s, remaining input: \"%s\", next char: %s]" pos rule label rem (cToHex rem.[0])
          else
              printfn "[failure at pos %d in rule %s: %s, remaining input: \"%s\", next char: EOF]" pos rule label rem
      o
  // if debugging is disabled
  else
      p i

let is_regexp(s: string)(rgx: string) =
  Regex.Match(s, rgx).Success

let is_whitespace(c: char) = is_regexp (c.ToString()) @"\s"

let is_whitespace_no_nl(c: char) = is_regexp (c.ToString()) @"\t| "

let is_upper(c: char) = is_regexp (c.ToString()) @"[A-Z]"

let is_lower(c: char) = is_regexp (c.ToString()) @"[a-z]"

let is_letter(c: char) = is_upper c || is_lower c

let is_digit(c: char) = is_regexp (c.ToString()) @"[0-9]"

let presult(a: 'a)(i: Input) : Outcome<'a> = Success(a,i)

let pzero(i: Input) : Outcome<'a> = Failure((position i), "pzero")

let pitem(i: Input) : Outcome<char> =
  let pos = position i
  let istr = input i
  if pos >= String.length istr then
      Failure ((position i),"pitem")
  else
      let debug = isDebug i
      let pos = position i
      Success (istr.[pos], (istr, pos + 1, debug))

let pbind(p: Parser<'a>)(f: 'a -> Parser<'b>)(i: Input) : Outcome<'b> =
  match p i with
  | Success(a,i') -> f a i'
  | Failure(pos,rule) -> Failure(pos,rule)

let pseq(p1: Parser<'a>)(p2: Parser<'b>)(f: 'a*'b -> 'c) : Parser<'c> =
  pbind p1 (fun a ->
      pbind p2 (fun b ->
          presult (f (a,b))
      )
  )

let pseq3(p1: Parser<'a>)(p2: Parser<'b>)(p3: Parser<'c>)(f: 'a*'b*'c -> 'd) : Parser<'d> =
  pbind p1 (fun a ->
      pbind p2 (fun b ->
          pbind p3 (fun c -> 
            presult (f (a,b,c))
          )
      )
  )

let psat(f: char -> bool) : Parser<char> =
  pbind pitem (fun c -> if (f c) then presult c else pzero)

let pchar(c: char) : Parser<char> = psat (fun c' -> c' = c)

let pletter : Parser<char> = psat is_letter

let pdigit : Parser<char> = psat is_digit

let pupper : Parser<char> = psat is_upper

let plower : Parser<char> = psat is_lower

let (<|>)(p1: Parser<'a>)(p2: Parser<'a>)(i: Input) : Outcome<'a> =
  let o = p1 i
  match o with
  | Success(_,_)      -> o
  | Failure(pos,rule) ->
      let o2 = p2 i
      match o2 with
      | Success(_,_)  -> o2
      | Failure(pos2,rule2) ->
          // return the furthest failure
          if pos >= pos2 then
              Failure(pos,rule)
          else
              Failure(pos2,rule2)

let pfun(p: Parser<'a>)(f: 'a -> 'b)(i: Input) : Outcome<'b> =
  let o = p i
  match o with
  | Success(a,i') -> Success(f a, i')
  | Failure(pos,rule) -> Failure(pos,rule)

let (|>>)(p: Parser<'a>)(f: 'a -> 'b) : Parser<'b> = pfun p f

let pfresult(p: Parser<'a>)(x: 'b) : Parser<'b> =
  pbind p (fun a -> presult x)

let rec pmany0(p: Parser<'a>)(i: Input) : Outcome<'a list> =
  let rec pm0(xs: 'a list)(i: Input) : Outcome<'a list> =
      match p i with
      | Failure(pos,rule) -> Success(xs, i)
      | Success(a, i')    ->
          if i = i' then
              failwith "pmany parser loops infinitely!"
          pm0 (a::xs) i'
  match pm0 [] i with
  | Success(xs,i')    -> Success(List.rev xs, i')
  | Failure(pos,rule) -> Failure(pos,rule)

let pmany1(p: Parser<'a>) : Parser<'a list> =
  pseq p (pmany0 p) (fun (x,xs) -> x :: xs)

let pwsNoNL0 : Parser<char list> = pmany0 (psat is_whitespace_no_nl)

let pwsNoNL1 : Parser<char list> = pmany1 (psat is_whitespace_no_nl)

let pws0 : Parser<char list> = pmany0 (psat is_whitespace)

let pws1 : Parser<char list> = pmany1 (psat is_whitespace)

let pstr(s: string) : Parser<string> =
  s.ToCharArray()
  |> Array.fold (fun pacc c ->
                    pseq pacc (pchar c) (fun (s,ch) -> s + ch.ToString())
                ) (presult "")

let pnl : Parser<string> =
  (psat (fun c -> c = '\n') |>> (fun c -> c.ToString()))
  <|> (pstr "\r\n")

let peof(i: Input) : Outcome<bool> =
  match pitem i with
  | Failure(pos,rule) ->
      if isEOF i then
          Success(true, i)
      else
          Failure(pos, rule)
  | Success(_,_) -> Failure((position i), "peof")

let pleft(pleft: Parser<'a>)(pright: Parser<'b>) : Parser<'a> =
  pbind pleft (fun a -> pfresult pright a)

let pright(pleft: Parser<'a>)(pright: Parser<'b>) : Parser<'b> =
  pbind pleft (fun _ -> pright)

let pbetween(popen: Parser<'a>)(pclose: Parser<'b>)(p: Parser<'c>) : Parser<'c> =
  pright popen (pleft p pclose)

let stringify(cs: char list) : string = String.Join("", cs)

let rec leftpad str ch num =
  if num > 0 then
      leftpad (ch.ToString() + str) ch (num - 1) 
  else
      str

let windowLeftIndex(window_sz: int)(failure_pos: int) : int =
  if failure_pos - window_sz < 0 then
      0
  else
      failure_pos - window_sz

let windowRightIndex(window_sz: int)(failure_pos: int)(buffer_len: int) : int =
  if failure_pos + window_sz >= buffer_len then
      buffer_len - 1
  else
      failure_pos + window_sz

let indexOfLastNewlineLeftWindow(left_index: int)(failure_pos: int)(buffer: string) : int =
  // search for last occurrence of '\n'
  let rec searchBackward(pos: int) : int option =
      if pos <= left_index then
          None
      else if buffer.[pos] = '\n' then
          Some pos
      else
          searchBackward (pos - 1)

  match searchBackward (failure_pos - 1) with
  | Some idx -> idx
  | None -> left_index

let indexOfFirstNewlineRightWindow(right_index: int)(failure_pos: int)(buffer: string) : int =
  // search for first occurrence of '\n'
  let rec searchForward(pos: int) : int option =
      if pos >= right_index then
          None
      else if buffer.[pos] = '\n' then
          Some pos
      else
          searchForward (pos + 1)

  match searchForward (failure_pos + 1) with
  | Some idx -> idx
  | None -> right_index

(*
 * Produce a diagnostic message for a parser failure.
 * @param window_sz The amount of context (in chars) to show to the left and right of the failure position.
 * @param failure_pos Where the parse failed.
 * @param buffer The input stream.
 * @param err The error message.
 *)
let diagnosticMessage(window_sz: int)(failure_pos: int)(buffer: string)(err: string) : string =
  // compute window
  let left_idx = windowLeftIndex window_sz failure_pos
  let right_idx = windowRightIndex window_sz failure_pos buffer.Length
  let last_nl_left = indexOfLastNewlineLeftWindow left_idx failure_pos buffer
  let first_nl_right = indexOfFirstNewlineRightWindow right_idx failure_pos buffer

  // find caret position in last line
  let caret_pos = failure_pos - last_nl_left

  // create window string
  let window = buffer.Substring(left_idx, failure_pos - left_idx + 1 + right_idx - failure_pos)

  // augment with diagnostic info
  let diag = err + "\n\n" + window + "\n" + (leftpad "^" ' ' (caret_pos - 1)) + "\n"

  diag