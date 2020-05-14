(* This program is an evaluator for friendship bracelet patterns. *)
module ProgramInterpreter
open ProgramParser
(*
(* Evaluates a Knot *)
let keval e =
    match e with
        | LL _ -> "<< "
        | RR _ -> ">> "
        | RL _ -> "> "
        | LR _ -> "< "
        | SKIP _ -> "_ "

(* Evaluates a Row *)
let reval e =
    match e with
        | Row r ->
            r |> List.map keval
              |> List.fold (+) "\n"

(* Evaluates a Component *)
let rec ceval e =
    match e with
    | Block b ->
        b |> List.map reval
          |> List.fold (+) ""
    | Repeat (n,c) ->
        c |> List.map ceval
          |> List.fold (+) ""
          |> String.replicate n
    | Name s ->
        "Pattern Name: " + s + "\n"
    | Strings (n,s) ->
        "Colors: " + (s |> List.fold (fun acc x -> acc + x + ", ") "")

(* Evaluates a Pattern *)
let eval e =
    match e with
    | Pattern p ->
        p |> List.map ceval
          |> List.fold (+) ""
          *)