

module Parse


type ParseSource<'A> = int * 'A

type ParseResult<'S, 'A> =
    | Success of 'A * ParseSource<'S>
    | Failure of ParseSource<'S> 

type Parser<'S, 'A> =  ParseSource<'S> -> ParseResult<'S, 'A>

// bind : ma -> (a -> mb) -> mb
// mu : a -> ma
type ParserBuilder() =
    member this.Bind (p : Parser<'S, 'A>, g : 'A -> Parser<'S, 'B>) = 
        (fun s -> 
            match p s with 
            | Success( value, source ) -> g value source
            | Failure source -> Failure source ) : Parser<'S, 'B>
            
    member this.Return a = fun source -> Success( a, source )

let parse = new ParserBuilder()

let getChar : Parser<char array, char> = fun s ->
    let (index, array) = s in
        if array.[index] = 'a' then Success( 'a', (index, array) )
        else Failure( index, array ) 

let z (*: Parser<char array, char * char>*) = 
    parse { let! a = getChar 
            let! b = getChar 
            return (a, b) 
          }

