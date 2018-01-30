
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

// **************** pNull **************** 

let pNull = 
    pstring "null"
    |>> (fun _ -> JNull)
    |?> "null parser"

// **************** pBool **************** 
let pBool =

    let ptrue = 
        pstring "true"
        |>> (fun _ -> true)

    let pfalse = 
        pstring "false"
        |>> (fun _ -> false)

    ptrue <|> pfalse
    |>> JBool
    |?> "bool parser"

// **************** pjstring **************** 

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

let quotedString =
    let quote = pchar '\"' 
    let jchar = pUnescapedChar <|> pescapedChar <|> punicodeChar

    quote >>. many jchar .>> quote |?> "quoted string" |>> (fun r -> String(List.toArray r))
    
let pjstring =
    quotedString |?> "quoted string" |>> JString


// **************** pNumber **************** 

let toJNumber (((optsign,digit),optfraction),optexp) =
    let (|*>) opt f =
        match opt with
        | Some x -> f x
        | _ -> ""

    let signstr = optsign |*> string

    let fracstr = optfraction |*> (fun s -> "." + s)

    let expstr = optexp |*> (fun (optSign, digits) ->  "e" + (optSign |*> Char.ToString) + digits)

    (signstr + digit + fracstr + expstr) |> float |> JNumber 

let negative = pchar '-' |?> "sign"

let positive = pchar '+' |?> "sign"

let zero = pstring "0" |?> "zero"

let digits = many pdigitChar |>> (fun c -> String(List.toArray c)) |?> "digits"

let fraction = pchar '.' >>. digits |?> "fraction"

let exponent = (pchar 'e' <|> pchar 'E') >>. optional ( negative <|> positive ) .>>. digits |?> "exponent"

let pNumber =
    optional negative .>>.  (zero <|> digits) .>>. (optional fraction) .>>. (optional exponent) |?> "Number" |>> toJNumber