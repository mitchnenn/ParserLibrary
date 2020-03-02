module Tests

open System
open Xunit
open ROP
open CharP
open RunP
open Parse

let parseDigit = anyOf ['0'..'9']

[<Fact>]
let ``Transform content test`` () =
    // Arrange.
    let parseThreeDigits =
        parseDigit .>>. parseDigit .>>. parseDigit
    // Act.
    let result = run parseThreeDigits "123A"
    // Assert.
    Assert.Equal(Success ((('1', '2'), '3'), "A"), result)
        
let parseThreeDigitsAsStr =
    // create a parser that returns a tuple
    let tupleParser =
        parseDigit .>>. parseDigit .>>. parseDigit

    // create a function that turns the tuple into a string
    let transformTuple ((c1, c2), c3) =
        String [| c1; c2; c3 |]

    // use "map" to combine them
    mapP transformTuple tupleParser

[<Fact>]
let ``Test mapped parser`` () =
    let result = run parseThreeDigitsAsStr "123A"  // Success ("123", "A")
    Assert.Equal(Success ("123", "A"), result)
    
let parseThreeDigitsAsInt =
    mapP int parseThreeDigitsAsStr

[<Fact>]
let ``Test mapped to in parser`` () =
    let result = run parseThreeDigitsAsInt "123A"  // Success (123, "A")
    Assert.Equal(Success (123, "A"), result)
    
[<Fact>]
let ``Parse map tests part 2`` () =
    let result = run parseThreeDigitsAsStr "123A"
    Assert.Equal(Success ("123", "A"), result)
    
[<Fact>]
let ``Parse map test part 2 integers`` () =
    let result = run parseThreeDigitsAsInt "123A"
    Assert.Equal(Success (123, "A"), result)
        
[<Fact>]
let ``Test list of three parsers`` () =
    let parsers = [ pchar 'A'; pchar 'B'; pchar 'C' ]
    let combined = sequence parsers
    
    let result = run combined "ABCD"
    
    Assert.Equal(Success (['A';'B';'C'], "D"), result)

let charListToStr charList =
    String(List.toArray charList)

let pstring str =
    str
    // convert to list of char
    |> List.ofSeq
    // map each char to a pchar
    |> List.map pchar
    // convert to Parser<char list>
    |> sequence
    // convert Parser<char list> to Parser<string>
    |> mapP charListToStr
    
[<Fact>]
let ``Test normal to normal world parsing`` () =
    let parseABC = pstring "ABC"
    let r1 = run parseABC "ABCDE"
    Assert.Equal(Success ("ABC", "DE"), r1)
    let r2 = run parseABC "A|CDE"
    Assert.Equal(Failure ("Expecting 'B'. Got '|'."), r2)
    let r3 = run parseABC "AB|DE"
    Assert.Equal(Failure ("Expecting 'C'. Got '|'."), r3)
    
[<Fact>]
let ``Test many function`` () =
    let manyA = many (pchar 'A')
    
    let r1 = run manyA "ABCD"
    Assert.Equal(Success (['A'], "BCD"), r1)
    let r2 = run manyA "AACD"
    Assert.Equal(Success (['A';'A'], "CD"), r2)
    let r3 = run manyA "AAAD"
    Assert.Equal(Success (['A';'A';'A'], "D"), r3)
    let r4 = run manyA "|BCD"
    Assert.Equal(Success ([], "|BCD"), r4)
    
    let manyAB = many (pstring "AB")
    
    let r5 = run manyAB "ABCD"
    Assert.Equal(Success (["AB"], "CD"), r5)
    let r6 = run manyAB "ABABCD"
    Assert.Equal(Success (["AB";"AB"], "CD"), r6)
    let r7 = run manyAB "ZCD"
    Assert.Equal(Success ([], "ZCD"), r7)
    let r8 = run manyAB "AZCD"
    Assert.Equal(Success ([], "AZCD"), r8)
    
    let whitespaceChar = anyOf [' '; '\t'; '\n']
    let whitespace = many whitespaceChar
    
    let r9 = run whitespace "ABC"
    Assert.Equal(Success ([], "ABC"), r9)
    let r10 = run whitespace " ABC"
    Assert.Equal(Success ([' '], "ABC"), r10)
    let r11 = run whitespace "\tABC"
    Assert.Equal(Success (['\t'], "ABC"), r11)
    
[<Fact>]
let ``Test many1 function`` () =
    let digit = anyOf ['0'..'9']
    let digits = many1 digit
    let r1 = run digits "1ABC"
    Assert.Equal(Success (['1'], "ABC"), r1)
    let r2 = run digits "12BC"
    Assert.Equal(Success (['1';'2'], "BC"), r2)
    let r3 = run digits "123C"
    Assert.Equal(Success (['1';'2';'3'], "C"), r3)
    let r4 = run digits "1234"
    Assert.Equal(Success (['1';'2';'3';'4'], ""), r4)
    let r5 = run digits "ABC"
    Assert.Equal(Failure "Expecting '9'. Got 'A'.", r5)

let pint =
    // helper
    let resultToInt (sign,charList) =
        let i = String(List.toArray charList) |> int
        match sign with
        | Some ch -> -i // negate the int
        | None -> i
        
    // define parse for one digit
    let digit = anyOf ['0'..'9']
    
    // define parser for one or more digits
    let digits = many1 digit
    
    // parse and convert
    opt (pchar '-') .>>. digits
    |>> resultToInt
    
[<Fact>]
let ``Test parse int`` () =
    let r1 = run pint "1ABC"
    Assert.Equal(Success (1, "ABC"), r1)
    let r2 = run pint "12BC"
    Assert.Equal(Success (12, "BC"), r2)
    let r3 = run pint "123C"
    Assert.Equal(Success (123, "C"), r3)
    let r4 = run pint "1234"
    Assert.Equal(Success (1234, ""), r4)
    
    let r5 = run pint "ABC"
    Assert.Equal(Failure "Expecting '9'. Got 'A'.", r5)
    
[<Fact>]
let ``Test optional character`` () =
    let digit = anyOf ['0'..'9']
    let digitThenSemicolon = digit .>>. opt(pchar ';')
    
    let r1 = run digitThenSemicolon "1;"
    Assert.Equal(Success (('1', Some ';'), ""), r1)
    let r2 = run digitThenSemicolon "1"
    Assert.Equal(Success (('1', None), ""), r2)
    
[<Fact>]
let ``Test negitive numbers`` () =
    let r1 = run pint "123C"
    Assert.Equal(Success (123, "C"), r1)
    let r2 = run pint "-123C"
    Assert.Equal(Success (-123, "C"), r2)
    
[<Fact>]
let ``Test ignore characters`` () =
    let whiteSpaceChar = anyOf [' '; '\t'; '\n']
    let whitespace = many1 whiteSpaceChar
    
    let ab = pstring "AB"
    let cd = pstring "CD"
    let ab_cd = (ab .>> whitespace) .>>. cd
    
    let r1 = run ab_cd "AB \t\nCD"
    Assert.Equal(Success (("AB", "CD"), ""), r1)
    
      
[<Fact>]
let ``Test between parsing`` () =
    let doublequote = pchar '"'
    let quotedInteger = between doublequote pint doublequote
    
    let r1 = run quotedInteger "\"1234\""
    Assert.Equal(Success (1234, ""), r1)
    let r2 = run quotedInteger "1234"
    Assert.Equal(Failure "Expecting '\"'. Got '1'.", r2)
    
[<Fact>]
let ``Test separated lists`` () =
    let comma = pchar ','
    let digit = anyOf ['0'..'9']
    
    let zeroOrMoreDigitList = sepBy digit comma
    let oneOrMoreDigitList = sepBy1 digit comma
    
    let r1 = run oneOrMoreDigitList "1;"
    Assert.Equal(Success (['1'], ";"), r1)
    let r2 = run oneOrMoreDigitList "1,2;"
    Assert.Equal(Success (['1';'2'], ";"), r2)
    let r3 = run oneOrMoreDigitList "1,2,3ABC"
    Assert.Equal(Success (['1';'2';'3'], "ABC"), r3)
    let r4 = run oneOrMoreDigitList "Z;ABC"
    Assert.Equal(Failure "Expecting '9'. Got 'Z'.", r4)
    
    let r5 = run zeroOrMoreDigitList "1;"
    Assert.Equal(Success (['1'], ";"), r5)
    let r6 = run zeroOrMoreDigitList "ZABC"
    Assert.Equal(Success ([], "ZABC"), r6)
    
