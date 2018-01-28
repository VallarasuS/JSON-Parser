
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