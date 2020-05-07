(* This program is an evaluator for friendship bracelet patterns. *)
module ProgramInterpreter
open ProgramParser

(**
let eval e =
    let rec evalrec e = // helper to actually parse the pattern
        match e with
        | Knot(k) ->
            match k with
            | LL -> "« "
            | RR -> "» "
            | RL -> "› "
            | LR -> "‹ "
        | Row(r) -> 
            r |> (List.map evalrec) + "/n"
    
    for i in 1 .. e.Length do // print string numbers
        printf "%d " i
        printf "\n"
    e |> List.map evalrec
**)