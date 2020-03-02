module RunP

open TypeP

/// Run a parser with some input
let run parser input = 
    // unwrap parser to get inner function
    let (Parser innerFn) = parser 
    // call inner function with input
    innerFn input
    
