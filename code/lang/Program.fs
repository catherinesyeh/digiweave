open System
open System.IO
open Parser
open ProgramParser
open ProgramInterpreter

[<EntryPoint>]
let main argv =
    let msg = "Usage: dotnet run <file.fbp>"
    if argv.Length <> 1 then // check for correct number of arguments
        printfn "%s" msg
        exit 1

    try
        let file = argv.[0] // read input from file
        let input = File.ReadAllText file

        match parse input with // try to parse expression
        | Some ast -> 
            let dir =  // get parent directory for output file
                Directory.GetParent(Directory.GetCurrentDirectory()).Parent.FullName
            printfn "%A" (eval ast dir) // time to evaluate!
            0
        | None -> 1
    with // handle exceptions
    | :? System.IO.FileNotFoundException as ex -> 
        printfn "File not found. %s" msg
        1