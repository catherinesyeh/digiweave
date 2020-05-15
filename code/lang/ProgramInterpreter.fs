(* This program is an evaluator for friendship bracelet patterns. *)
module ProgramInterpreter
open ProgramParser

(* Evaluates a Knot *)
let keval e s i =
    match e with // print corresponding symbol for each knot
    | RR _ -> List.item(i) s + " >> "
    | LL _ -> List.item(i+1) s + " << "
    | RL _ -> List.item(i) s + " > "
    | LR _ -> List.item(i+1) s + " < "
    | SKIP _ -> " _ "

// update list of colors to match string locations
let rec swap acc s i =
    if (List.length acc) < i then // keep strings up to i in the same order
      let newacc = List.append acc [List.head s]
      let news = List.tail s
      swap newacc news i
    else // swap strings at i and i + 1
      let temp1 = [List.head s]
      let news1 = List.tail s
      let temp2 = [List.head news1]
      let news2 = List.tail news1
      List.append (List.append acc (List.append temp2 temp1)) news2

// only swap strings if needed (aka. RR or LL knot)
let kstringshelperhelper e s i =
    match e with
    | RR _ -> swap [] s i
    | LL _ -> swap [] s i
    | RL _ -> s
    | LR _ -> s
    | SKIP _ -> s

// return updated list of strings 
let rec kstringshelper row s i output =
    if (List.isEmpty row) then // finished the row so return results
      (s, output)
    else // process the next knot
      let news = kstringshelperhelper (List.head row) s i // update strings
      let knot = keval (List.head row) s i 
      let newrow = List.tail row 
      kstringshelper newrow news (i+1) (output + knot)

(* Evaluates a Row *)
let reval e s =
    match e with
    | Row r ->
        let newstrings, output = kstringshelper r s 0 ""
        (newstrings, "\n" + output)

(* Evaluates a Block *)
let rec beval (e: Row list) s output =
    if (e.IsEmpty) then // finished the block so return results
      (s, output)
    else
      let tup = reval e.Head s // process a row
      match tup with
      | (newcolors, news) ->
          beval e.Tail newcolors (List.append output [news])

(* Evaluates a Component *)
let rec ceval e s =
    match e with
    | Block b -> // found a block
        let helper = beval b s []
        match helper with
        | (colors, output) ->
            let out = List.fold (+) "" output
            (colors, out)
    | Repeat (n,c) -> // found a repeat operation
        repeathelper n c s ""

// perform repeat operation
and repeathelper n e s acc =
    if n = 0 then // finished so return results
      (s, acc)
    else // repeat once and recurse
      let result = clisthelper e s ""
      match result with
      | (colors, output) ->
          let out = acc + output
          repeathelper (n-1) e colors out
 
// evaluates list of components
and clisthelper e s acc =
    if (e.IsEmpty) then // no more components in the list
      (s, acc)
    else // process next component
      let result = ceval e.Head s
      let newe = e.Tail
      match result with
      | (colors, output) ->
          let out = acc + output
          clisthelper newe colors out

(* Evaluates a Strings *)
let seval e =
    match e with
    | Strings (_, strings) ->
        strings // just return the list of colors in order

(* Evaluates a Name *)
let neval e =
    match e with
    | Name name ->
        "Pattern Name: " + name + "\n"

(* Evaluates a Pattern *)
let eval e =
    match e with
    | Pattern (name, strings, components) ->
        // evaluate the components in the pattern
        let comps = clisthelper components (seval strings) ""
        match comps with
        | (colors, fincomp) -> // put together name and evaluated components for fully evaluated pattern
            neval name + fincomp