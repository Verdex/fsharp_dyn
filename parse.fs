

module Parse


type ParseSource<'A> = int * 'A

type ParseResult<'S, 'A> =
    | Success of 'A * ParseSource<'S>
    | Failure of ParseSource<'S> 

type Parser<'S, 'A> = ParseSource<'S> -> ParseResult<'S, 'A>

// bind : ma -> (a -> mb) -> mb
// mu : a -> ma
(*type ParserBuilder() =
    member this.Bind p g =
    member this.Return a = *)
