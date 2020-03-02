module TypeP

open ROP

/// Type that wraps a parsing function
type Parser<'T> = Parser of (string -> Result<'T * string>)
