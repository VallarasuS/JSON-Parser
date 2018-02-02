# JSON-Parser
JSON-Parser written in f#

## Parsers

Smallest unit of JSON-Parser is a character parser (`pchar`), and it builds by incrementally combining smaller units to a complete set.

`pchar` takes a character as an input and returns a `Parser<char>` with the following singature.

```fsharp

char -> Parser<char>

```

An expample of incremental build up is a string parser (`pstring`) with the following singature.

```fsharp

seq<char> -> Parser<String>

```

Alternatively a parser can be built up by supplying a predicate like the one bellow to build a (`digit`) Parser.

```fsharp

satisfy Char.IsDigit "digit"

```

## Combinators

A limited necessary set of combinators (chaining and piping parsers) are included as well. 

`p1 .>>. p2`	- Applies p1 and p2 in sequence, and returns a tuple with results of p1 and p2.

`p1 >>. p2`	- Applies p1 and p2 in sequence, and returns result of p2.

`p1 .>> p2`	- Applies p1 and p2 in sequence, and returns result of p1.

`p1 <|> p2`	- Applies p1, p2 in sequence, and returns on succeess. Proceeds with p2 only when p1 fails.

`p |>> f`	- Applies p and returns f x.

`btw p1 p2 p3`	- Applies p1, p2 and p3 in sequence, and returns result of p2.



Source of inspiration [Scott Wlaschin](https://fsharpforfunandprofit.com/posts/understanding-parser-combinators/) and [Stephan Tolksdorf](https://github.com/stephan-tolksdorf/fparsec).
