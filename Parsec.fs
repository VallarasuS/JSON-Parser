module Parsec

open TextInput
open System

type ParserName = string
type ParserError = string

type Result<'a> =
    | Success of 'a
    | Failure of  ParserError * ParserName * Position

type Parser<'a> = {
    parse : (Input -> Result<'a * Input>)
    name : ParserName
}

// **************** Helpers **************** 

let run p input =
    p.parse input

let satisfy predicate label =
    let fn input =
        let remaining, c = nextChar input
        match c with
        | None -> Failure("End of String", label, TextInput.postionFromInput input)
        | Some c -> 
            if predicate c then
                Success(c, remaining)
            else
                let e = sprintf "unexpected %c" c
                Failure(e, label, postionFromInput input)
    { parse = fn; name = label }

let setLabel p l =
    let fn input = 
        match (run p input) with
        | Success (a, i) ->
            Success(a, i)
        | Failure(e, n, p) ->
            Failure(e,l,p)
    { parse = fn; name = l }
    
let (|?>) = setLabel 

let rec zeroOrMore p input acc = 
    let r = run p input
    match r with
    | Failure(e,n,p) -> 
        (acc, input)
    | Success(a, i) ->
        zeroOrMore p i (a :: acc)

// **************** Combinators **************** 

let bind f p =
    let fn input = 
        match (run p input) with
        | Success (a, i) ->
            Success((f a), i)
        | Failure(e, n, p) ->
            Failure(e,n,p)

    { parse = fn; name = "unknown" } 

let (|>>) x f = bind f x

let ndt p1 p2 =
    let fn input =
        match (run p1 input) with
        | Success (a, i) ->
          match (run p2 i) with
          | Success (r, inew) -> Success( (a,r), inew)
          | Failure(e,n,p) ->
            Failure(e,n,p) 
        | Failure(e,n,p) ->
            Failure(e,n,p) 

    { parse = fn; name = "unknown" }

let (.>>.) = ndt

let orelse p1 p2 =
    let fn input =
        match (run p1 input) with
        | Success (a, i) -> Success (a, i)
        | Failure (_,_,_) ->
            run p2 input

    { parse = fn; name = "unknown" }

let (<|>) = orelse

let returnp x =
    let fn input =
        Success (x, input)
    { parse = fn; name = "unknown" }

let (<*>) fp xp =
   ndt fp xp 
   |> bind (fun (f,x) -> f x)

let lift f xp yp =
    returnp f <*> xp <*> yp

let rec reducer plist =
    let cons h t = h::t
    let consp = lift cons

    match plist with
    | [] -> returnp []
    | h::t -> consp h (reducer t)

let (.>>) p1 p2 =
    p1 .>>. p2
    |>> (fun (a,b) -> a)

let (>>.) p1 p2 =
    p1 .>>. p2
    |>> (fun (a,b) -> b)

let btw p1 p2 p3 =
    p1 >>. p2 .>> p3

let many p =
    let fn input =
        Success(zeroOrMore p input [])
    { parse = fn; name = "unknown" }

// **************** CHAR PARSERS **************** 

let pchar c = 
    let predicate ch = (ch = c)
    let label = sprintf "%c" c
    satisfy predicate label
    
let digitChar =
    let predicate = Char.IsDigit
    let label = "digit"
    satisfy predicate label

let whitespaceChar =
    let predicate = Char.IsWhiteSpace
    let label = "whitespace"
    satisfy predicate label

let anyof plist =
    plist 
    |> List.map pchar 
    |> List.reduce orelse

// **************** STRING PARSERS **************** 

let pstring str = 
    str
    |> Seq.toList
    |> List.map pchar
    |> reducer
    |> bind (fun l -> String(List.toArray l))

