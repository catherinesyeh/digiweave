(* This program is an evaluator for friendship bracelet patterns. *)
module ProgramInterpreter
open ProgramParser

(* Evaluates a Knot *)
let keval e =
    match e with
        | LL _ -> "<< "
        | RR _ -> ">> "
        | RL _ -> "> "
        | LR _ -> "< "
        | SKIP _ -> "_ "

(* Evaluates a Row *)
let reval e s =
    match e with
        | Row r ->
            r |> List.map keval
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

(* Evaluates a Name *)
let neval e =
    match e with
    | Name name ->
        "Pattern Name: " + name + "\n"

(* Evaluates a Pattern *)
let eval e =
    match e with
    | Pattern (name, strings, components) ->
        neval name + (components |> List.map (fun x -> ceval x strings)
                                 |> List.fold (+) "")