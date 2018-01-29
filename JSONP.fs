
module JSONParser

open Parsec
open System

type JValue =
    | JString of string
    | JNumber of float
    | JObject of Map<string, JValue>
    | JArray of JValue list
    | JBool of bool
    | JNull

let pNull = 
    pstring "null"
    |>> (fun _ -> JNull)
    |?> "null parser"

let pBool =

    let ptrue = 
        pstring "true"
        |>> (fun _ -> true)

    let pfalse = 
        pstring "false"
        |>> (fun _ -> false)

    ptrue <|> pfalse
    |>> (fun b -> JBool b)
    |?> "bool parser"

let pUnescapedChar =
    let predicate = (fun c -> c <> '\\' && c <> '\"' )
    satisfy predicate "unescaped char"

// refer - http://www.json.org
let escapedChars = [
    ("\\\"", '\"')      // quote
    ("\\\\", '\\')      // reverse solidus
    ("\\/", '/')        // solidus
    ("\\b", '\b')       // backspace
    ("\\f", '\f')       // formfeed
    ("\\r", '\r')       // carriage return
    ("\\n", '\n')       // new line
    ("\\t", '\t')       // horizontal tab
]

let pescapedChar =  
    escapedChars
    |> List.map (fun (c, r) -> pstring c |>> (fun _ -> r ))
    |>  List.reduce orelse
    |?> "escaped char"

let punicodeChar =
    let backslash = pchar '\\'
    let uChar = pchar 'u'
    let clist = ['0'..'9'] @ ['A'..'F'] @ ['a'..'f'] 
    let hexdigit = anyof (clist)

    let toChar (((h1,h2),h3),h4) = 
        let str = sprintf "%c%c%c%c" h1 h2 h3 h4
        Int32.Parse(str, Globalization.NumberStyles.HexNumber) |> char
        
    backslash >>. uChar >>. hexdigit .>>. hexdigit .>>. hexdigit .>>. hexdigit |?> "unicode char" |>> toChar