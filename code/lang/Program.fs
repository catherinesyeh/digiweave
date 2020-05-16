open System
open System.IO
open Parser
open ProgramParser
open ProgramInterpreter

[<EntryPoint>]
let main argv =
    if argv.Length <> 1 then
        printfn "Usage: dotnet run <file.fbp>"
        exit 1
    
    // use this to test with string on command line (ex. "dotnet run AAA")
//    let input = argv.[0]

    // use this to test with file input (ex: dotnet run ../../examples/example-1.fbp)
    let file = argv.[0]
    let input = File.ReadAllText file

    match parse input with // try to parse expression
//    | Some ast -> printfn "%A" ast // use this to test raw parser output
    | Some ast -> printfn "%A" (eval ast) // use this to test interpreter output
    | None    -> printfn "Invalid expression."
    0 // return an integer exit code
