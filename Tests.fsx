#load "TextInput.fs"
#load "Parsec.fs"
#load "JSONP.fs"

open Parsec
open TextInput
open JSONParser
open System

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
fromString  inpStr
|> run parseAA 
|> print 
    
// **************** TEST pstring **************** 

let parseA1 = parseA |> many |>> (fun c -> String(Seq.toArray c))
fromString  inpStr
|> run parseA1
|> print

// **************** TEST pNull **************** 
    
fromString "null"
|> run pNull
|> print

// **************** TEST pBool **************** 

fromString "true"
|> run pBool
|> print

fromString "false"
|> run pBool
|> print

// **************** TEST pUnescapedChar **************** 

fromString "somestring"
|> run pUnescapedChar
|> print

fromString "\"somestring"
|> run pUnescapedChar
|> print

// **************** TEST pescapedChar **************** 

fromString "somestring"
|> run pescapedChar
|> print

fromString "\\b somestring"
|> run pescapedChar
|> print

// **************** TEST punicodeChar **************** 

fromString "\\u263A"
|> run punicodeChar
|> print

// **************** TEST pjstring **************** 

fromString "\"a\""
|> run pjstring
|> print

fromString "\"ab\\tde\"" 
|> run pjstring
|> print

fromString "\"ab\\u263Ade\""
|> run pjstring
|> print

// **************** TEST pNumber **************** 

fromString "-123"
|> run pNumber
|> print

fromString "123"
|> run pNumber
|> print

fromString "123.123e3"
|> run pNumber
|> print

fromString "123.123e-3"
|> run pNumber
|> print 

// **************** TEST pArray **************** 

fromString "[1, 2, 3]"
|> run pArray
|> print

// **************** TEST pObject **************** 

fromString "{ \"a\" : 1, \"b\":2 }"
|> run pObject
|> print