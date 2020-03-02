module Parse

open ROP
open TypeP
open CharP
open RunP

/// "bindP" takes a parser-producing function f, and a parser p
/// and passes the output of p into f, to create a new parser
let bindP f p =
    let innerFn input =
        let result1 = run p input 
        match result1 with
        | Failure err -> 
            // return error from parser1
            Failure err  
        | Success (value1,remainingInput) ->
            // apply f to get a new parser
            let p2 = f value1
            // run parser with remaining input
            run p2 remainingInput
    Parser innerFn 

/// Infix version of bindP
let ( >>= ) p f = bindP f p

/// Lift a value to a Parser
let returnP x = 
    let innerFn input =
        // ignore the input and return x
        Success (x,input)
    // return the inner function
    Parser innerFn 

/// apply a function to the value inside a parser
let mapP f = 
    bindP (f >> returnP)

/// infix version of mapP
let ( <!> ) = mapP

/// "piping" version of mapP
let ( |>> ) x f = mapP f x

/// apply a wrapped function to a wrapped value
let applyP fP xP =         
    fP >>= (fun f -> 
    xP >>= (fun x -> 
        returnP (f x) ))

/// infix version of apply
let ( <*> ) = applyP

/// lift a two parameter function to Parser World
let lift2 f xP yP =
    returnP f <*> xP <*> yP

/// Combine two parsers as "A andThen B"
let andThen p1 p2 =         
    p1 >>= (fun p1Result -> 
    p2 >>= (fun p2Result -> 
        returnP (p1Result,p2Result) ))

/// Infix version of andThen
let ( .>>. ) = andThen

/// Combine two parsers as "A orElse B"
let orElse p1 p2 =
    let innerFn input =
        // run parser1 with the input
        let result1 = run p1 input

        // test the result for Failure/Success
        match result1 with
        | Success result -> 
            // if success, return the original result
            result1

        | Failure err -> 
            // if failed, run parser2 with the input
            let result2 = run p2 input

            // return parser2's result
            result2 

    // return the inner function
    Parser innerFn 

/// Infix version of orElse
let ( <|> ) = orElse

/// Choose any of a list of parsers
let choice listOfParsers = 
    List.reduce ( <|> ) listOfParsers 

/// Choose any of a list of characters
let anyOf listOfChars = 
    listOfChars
    |> List.map pchar // convert into parsers
    |> choice

/// Convert a list of Parsers into a Parser of a list
let rec sequence parserList =
    // define the "cons" function, which is a two parameter function
    let cons head tail = head::tail

    // lift it to Parser World
    let consP = lift2 cons

    // process the list of parsers recursively
    match parserList with
    | [] -> 
        returnP []
    | head::tail ->
        consP head (sequence tail)

/// (helper) match zero or more occurences of the specified parser
let rec parseZeroOrMore parser input =
    // run parser with the input
    let firstResult = run parser input 
    // test the result for Failure/Success
    match firstResult with
    | Failure err -> 
        // if parse fails, return empty list
        ([],input)  
    | Success (firstValue,inputAfterFirstParse) -> 
        // if parse succeeds, call recursively
        // to get the subsequent values
        let (subsequentValues,remainingInput) = 
            parseZeroOrMore parser inputAfterFirstParse
        let values = firstValue::subsequentValues
        (values,remainingInput)  

/// matches zero or more occurences of the specified parser
let many parser = 

    let rec innerFn input =
        // parse the input -- wrap in Success as it always succeeds
        Success (parseZeroOrMore parser input)

    Parser innerFn

/// matches one or more occurences of the specified parser
let many1 p =         
    p      >>= (fun head -> 
    many p >>= (fun tail -> 
        returnP (head::tail) ))

/// Parses an optional occurrence of p and returns an option value.
let opt p = 
    let some = p |>> Some
    let none = returnP None
    some <|> none

/// Keep only the result of the left side parser
let (.>>) p1 p2 = 
    // create a pair
    p1 .>>. p2 
    // then only keep the first value
    |> mapP (fun (a,b) -> a) 

/// Keep only the result of the right side parser
let (>>.) p1 p2 = 
    // create a pair
    p1 .>>. p2 
    // then only keep the second value
    |> mapP (fun (a,b) -> b) 

/// Keep only the result of the middle parser
let between p1 p2 p3 = 
    p1 >>. p2 .>> p3 

/// Parses one or more occurrences of p separated by sep
let sepBy1 p sep =
    let sepThenP = sep >>. p            
    p .>>. many sepThenP 
    |>> fun (p,pList) -> p::pList

/// Parses zero or more occurrences of p separated by sep
let sepBy p sep =
    sepBy1 p sep <|> returnP []