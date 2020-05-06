// Learn more about F# at http://fsharp.org

open System
open Parser
open ProgramParser
open ProgramInterpreter

[<EntryPoint>]
let main argv =
    (* trick for later
    Console.ForegroundColor <- ConsoleColor.Blue // change the color of text!
    Console.WriteLine("Hello World from F#!")
     *)

    if argv.Length <> 1 then
        printfn "Usage: dotnet run <pattern>"
        exit 1

    let input = argv.[0]
    match parse input with // try to parse expression
    | Some ast -> printfn "%A" ast
    | None    -> printfn "Invalid expression."
    0 // return an integer exit code
