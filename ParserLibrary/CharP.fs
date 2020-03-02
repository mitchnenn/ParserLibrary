module CharP

open System
open ROP
open TypeP

/// Parse a single character
let pchar charToMatch = 
    // define a nested inner function
    let innerFn str =
        if String.IsNullOrEmpty(str) then
            Failure "No more input"
        else
            let first = str.[0] 
            if first = charToMatch then
                let remaining = str.[1..]
                Success (charToMatch,remaining)
            else
                let msg = sprintf "Expecting '%c'. Got '%c'." charToMatch first
                Failure msg
    // return the "wrapped" inner function
    Parser innerFn
    
