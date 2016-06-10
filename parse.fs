

module Parse


type ParseSource<'A> = int * 'A

type ParseResult<'S, 'A> =
    | Success of 'A * ParseSource<'S>
    | Failure of ParseSource<'S> 

type Parser<'S, 'A> = Parser of ( ParseSource<'S> -> ParseResult<'S, 'A> )

let parseWith (Parser f) v = f v

// bind : ma -> (a -> mb) -> mb
// mu : a -> ma
type ParserBuilder() =
    member this.Bind<'S, 'A, 'B> (p : Parser<'S, 'A>) (g : 'A -> Parser<'S, 'B>) = 
        Parser( fun (s : ParseSource<'S>) -> 
            match parseWith p s with 
            | Success( value, source ) -> parseWith (g value) source
            | Failure source -> Failure source )
            
    member this.Return a = Parser( fun source -> Success( a, source ) )

let parse = new ParserBuilder()

let getChar : Parser<char array, char> = Parser( fun s ->
    let (index, array) = s in
        if array.[index] = 'a' then Success( 'a', (index, array) )
        else Failure( index, array ) )

let z (*: Parser<char array, char * char>*) = 
    parse { let! a = getChar 
            let! b = getChar 
            return (a, b) 
          }

