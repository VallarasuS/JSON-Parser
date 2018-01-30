open System

open Parsec
open TextInput
open JSONParser

[<EntryPoint>]
let main argv = 

    let rec printl jv =
        match jv with
        | JString s -> printfn  "   JString %s" s
        | JNumber f -> printfn "    JNumber %f" f
        | JBool b -> printfn "  JBool %b" b
        | JNull -> printfn "    JNull"
        | JArray al -> 
            match al with
            | h::t 
                -> printl h; printl (JArray t)
            | _ -> printfn "end of array"
        | JObject m ->
            let k =  m |> Map.toList
            match k with
            | (key,value)::t -> printfn "%s" key; printl value; printl (JObject (t |> Map.ofList))
            | _ -> printfn "end of map"
    
    let print result = 
        match result with
        | Failure(e,n,p) -> printfn "Error   : %s in %s" e n
        | Success (r,_) -> 
            match box r with
            | :? JValue as v -> printl v
            | _ -> printfn "Success : %s" (r.ToString())

    let unwrap r input =
        match r with
        | Success(a,i) -> i
        | Failure(e,n, p) -> { input with position = p }
(*
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
    
// **************** TEST pstring **************** 

    let parseA1 = parseA |> many |>> (fun c -> String(Seq.toArray c))
    let input = fromString  inpStr
    let result = run parseA1 input
    print result

// **************** TEST pNull **************** 
    
    let input = fromString "null"
    let result = run pNull input
    print result

// **************** TEST pBool **************** 

    let input = fromString "true"
    let result = run pBool input
    print result

    let input = fromString "false"
    let result = run pBool input
    print result

// **************** TEST pUnescapedChar **************** 

    let input = fromString "somestring"
    let result = run pUnescapedChar input
    print result

    let input = fromString "\"somestring"
    let result = run pUnescapedChar input
    print result

// **************** TEST pescapedChar **************** 

    let input = fromString "somestring"
    let result = run pescapedChar input
    print result

    let input = fromString "\\b somestring"
    let result = run pescapedChar input
    print result

// **************** TEST punicodeChar **************** 

    let input = fromString "\\u263A"
    let result = run punicodeChar input
    print result

// **************** TEST pjstring **************** 

    let i = fromString "\"a\""
    let ri = run pjstring i
    print ri

    let input = fromString "\"ab\\tde\"" 
    let result = run pjstring input
    print result

    let input = fromString "\"ab\\u263Ade\""
    let result = run pjstring input
    print result

// **************** TEST pNumber **************** 

    let input = fromString "-123"
    let result = run pNumber input
    print result

    let input = fromString "123"
    let result = run pNumber input
    print result

    let input = fromString "123.123e3"
    let result = run pNumber input
    print result

    let input = fromString "123.123e-3"
    let result = run pNumber input
    print result

// **************** TEST pArray **************** 

    let input = fromString "[1, 2, 3]"
    let result = run pArray input
    print result

// **************** TEST pObject **************** 

    let input = fromString "{ \"a\" : 1, \"b\":2 }"
    let result = run pObject input
    print result
*)

// **************** finally TEST comple parser **************** 

    let input = fromString "{ \"name\" : \"Vallarasu\", \"gender\" : \"male\", \"bday\" : {\"year\":2001, \"month\":12, \"day\":25 }, \"favouriteColors\" : [\"blue\", \"green\"], \"IsEmployed\" : true}"
    let result = run pjvalue input
    print result

    Console.ReadLine() |> ignore

    0