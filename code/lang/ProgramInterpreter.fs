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
      let temp2 = [List.head s]
      let news2 = List.tail news1
      List.append (List.append acc (List.append temp2 temp1)) news2

let kstringshelperhelper e s i =
    match e with
        | RR _ -> swap [] s i
        | LL _ -> swap [] s (i-1)
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
            r |> List.mapi (fun i x -> keval x newstrings i)
              |> List.fold (+) "\n"

(* Evaluates a Component *)
let rec ceval e s =
    match e with
    | Block b ->
        b |> List.map (fun x -> reval x s)
          |> List.fold (+) ""
    | Repeat (n,c) ->
        c |> List.map (fun x -> ceval x s)
          |> List.fold (+) ""
          |> String.replicate n

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
        neval name + (components |> List.map (fun x -> ceval x (seval strings))
                                 |> List.fold (+) "")