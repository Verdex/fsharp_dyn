

module Parse


type ParseSource<'a> = int * 'a

type ParseResult<'s, 'a>  =
    | Success of 'a * ParseSource<'s>
    | Failure

type Parser<'s, 'a> = ParseSource<'s> -> ParseResult<'s, 'a>

// bind : ma -> (a -> mb) -> mb
// mu : a -> ma
(*type ParserBuilder() =
    member this.Bind p g =
    member this.Return a = *)
