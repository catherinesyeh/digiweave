(* This program is an evaluator for friendship bracelet patterns. *)
module ProgramInterpreter
open ProgramParser

(* Evaluates a Knot *)
let keval e s i =
    match e with
    | RR _ -> List.item(i) s + " >> "
    | LL _ -> List.item(i+1) s + " << "
    | RL _ -> List.item(i) s + " > "
    | LR _ -> List.item(i+1) s + " < "
    | SKIP _ -> " _ "

let rec swap acc s i =
    if (List.length acc) < i then
      let newacc = List.append acc [List.head s]
      let news = List.tail s
      swap newacc news i
    else
      let temp1 = [List.head s]
      let news1 = List.tail s
      let temp2 = [List.head news1]
      let news2 = List.tail news1
      List.append (List.append acc (List.append temp2 temp1)) news2

let kstringshelperhelper e s i =
    match e with
    | RR _ -> swap [] s i
    | LL _ -> swap [] s i
    | RL _ -> s
    | LR _ -> s
    | SKIP _ -> s

let rec kstringshelper row s i =
    if (List.isEmpty row) then
      s
    else
      let news = kstringshelperhelper (List.head row) s i
      let newrow = List.tail row
      kstringshelper newrow news (i+1)

(* Evaluates a Row *)
let reval e s =
    match e with
    | Row r ->
        let newstrings = kstringshelper r s 0
        r |> List.mapi (fun i x -> keval x s i)
          |> List.fold (+) "\n"
          |> (fun a b -> (a, b)) newstrings

let rec cevalhelper (e: Row list) s output =
    if (e.IsEmpty) then
      (s, output)
    else
      let tup = reval e.Head s
      match tup with
      | (newcolors, news) ->
          cevalhelper e.Tail newcolors (List.append output [news])

(* Evaluates a Component *)
let rec ceval e s =
    match e with
    | Block b ->
        let helper = cevalhelper b s []
        match helper with
        | (colors, output) ->
            let out = List.fold (+) "" output
            (colors, out)
    | Repeat (n,c) ->
        repeathelper n c s ""

and repeathelper n e s acc =
    if n = 0 then
      (s, acc)
    else
      let result = clisthelper e s ""
      match result with
      | (colors, output) ->
          let out = acc + output
          repeathelper (n-1) e colors out
           
and clisthelper e s acc =
    if (e.IsEmpty) then
      (s, acc)
    else
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
        strings

(* Evaluates a Name *)
let neval e =
    match e with
    | Name name ->
        "Pattern Name: " + name + "\n"

(* Evaluates a Pattern *)
let eval e =
    match e with
    | Pattern (name, strings, components) ->
        let comps = clisthelper components (seval strings) ""
        match comps with
        | (colors, fincomp) ->
            neval name + fincomp