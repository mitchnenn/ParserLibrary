module ROP

/// Type that represents Success/Failure in parsing
type Result<'a> =
    | Success of 'a
    | Failure of string
    