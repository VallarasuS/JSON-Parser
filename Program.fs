open System

open Parsec
open TextInput
open JSONParser

[<EntryPoint>]
let main argv = 
    
    let print result = 
        match result with
        | Failure(e,n,p) -> printfn "%s in %s" e n
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

    let input = fromString "null";
    let result = run pNull input
    print result

// **************** TEST pBool **************** 

    let input = fromString "true";
    let result = run pBool input
    print result

    let input = fromString "false";
    let result = run pBool input
    print result

// **************** TEST pUnescapedChar **************** 

    let input = fromString "somestring";
    let result = run pUnescapedChar input
    print result

    let input = fromString "\"somestring";
    let result = run pUnescapedChar input
    print result

// **************** TEST pescapedChar **************** 

    let input = fromString "somestring";
    let result = run pescapedChar input
    print result

    let input = fromString "\\b somestring";
    let result = run pescapedChar input
    print result

// **************** TEST punicodeChar **************** 

    let input = fromString "\\u263A";
    let result = run punicodeChar input
    print result

    Console.ReadLine() |> ignore

    0