module TextInput

open System

type Position = {
    currentLine : string
    line : int
    col : int 
}

type Input = {
    lines: string []
    position : Position 
}

let initPosition : Position = { line = 0; col = 0; currentLine = "" }

let fromString (str:String) = 
    let sep = [| "\r\n"; "\n" |]
    let lines = str.Split(sep, StringSplitOptions.None)
    { lines = lines ; position = initPosition }

let currentLine input = 
    let line = input.position.line
    let col = input.position.col

    if (line < input.lines.Length) then
        input.lines.[line]
    else
        "end of file"

let incrCol position = 
    { position with col = position.col + 1 }
 
let incrLine position = 
    { position with line = position.line + 1 }   

let nextChar input =
    let line = input.position.line
    let col = input.position.col

    if(line >= input.lines.Length) then
        input, None
    else
        let currentLine = currentLine input
        if (col < currentLine.Length) then
            let c = currentLine.[col]
            let newPos = incrCol input.position
            let newState = { input with position = newPos }
            newState, Some c
        else
            let c = '\n'
            let newPos = incrLine input.position
            let newState = { input with position = newPos }
            newState, Some c

let postionFromInput input = {
    currentLine = currentLine input
    line = input.position.line
    col = input.position.col
}
