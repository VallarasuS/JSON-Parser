open Parsec
open TextInput
open System

[<EntryPoint>]
let main argv = 
    
    let print result = 
        match result with
        | Failure(e,n,p) -> printfn "%s" e
        | Success (r,_) -> printfn "%s" (r.ToString())

    let unwrap r input =
        match r with
        | Success(a,i) -> i
        | Failure(e,n, p) -> { input with position = p }

// **************** TEST pchar **************** 

    let inpStr = "aabcde..f2344"

    let parseA = pchar 'a'
    let input = fromString  inpStr;
    let result = run parseA input
    print result

    let i = unwrap result input
    let result1 = run parseA i
    print result1

// **************** TEST pstring **************** 

    let parseAA = pstring "aa"
    let input = fromString  inpStr;
    let result = run parseAA input
    print result

    Console.ReadLine() |> ignore

    0