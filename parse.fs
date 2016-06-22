

module Parse


type ParseSource<'a> = int * 'a

type ParseResult<'s, 'a> =
    | Success of 'a * ParseSource<'s>
    | Failure of ParseSource<'s> 

type Parser<'s, 'a> =  ParseSource<'s> -> ParseResult<'s, 'a>

type ParserBuilder() =
    member this.Bind (p : Parser<'s, 'a>, g : 'a -> Parser<'s, 'b>) = 
        (fun s -> 
            match p s with 
            | Success( value, source ) -> g value source
            | Failure source -> Failure source ) 
            
    member this.Return a = fun source -> Success( a, source )

let map (f : 'a -> 'b)  (p : Parser<'s, 'a>)  = 
    fun (s : ParseSource<'s>) -> 
        match p s with
        | Success ( value, source ) -> Success( f value, source )
        | Failure source -> Failure source

let parse = new ParserBuilder()

let getChar : Parser<char array, char> = fun s ->
    let (index, array) = s in
        if array.[index] = 'a' then Success( 'a', (index, array) )
        else Failure( index, array ) 

let z = 
    parse { let! a = getChar 
            let! b = getChar 
            let! z = map (fun h -> h) getChar 
            return (a, b, z) 
          }

