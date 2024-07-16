module Bencode

open FParsec

type EncodedBencode = string

type Bencode =
    | BString of string
    | BInteger of int64
    | BList of Bencode list
    | BDictionary of Map<string, Bencode>

let charsToInt: char list -> int = System.String.Concat >> int

let matchExact len condition = manyMinMaxSatisfy len len condition

let parseBString: Parser<Bencode, unit> =
    parse {
        let! contentLength = manyTill digit (pchar ':') |>> charsToInt

        return! matchExact contentLength isLetter |>> BString
    }

let parseBInteger: Parser<Bencode, unit> =
    parse {
        let! _ = pchar 'i'
        let! sign = opt (pchar '-')
        let! decodedNumber = manyTill digit (pchar 'e') |>> charsToInt

        return
            match sign with
            | Some _ -> BInteger -decodedNumber
            | _ -> BInteger decodedNumber
    }

let rec parseList () : Parser<Bencode, unit> =
    parse {
        let! _ = pchar 'l'
        let parseListItem = parseValue () .>> optional (pchar ',')
        return! many parseListItem .>> pchar 'e' |>> BList
    }

and parseValue () : Parser<Bencode, unit> = choice [parseBInteger; parseBString; parseList ()]
// let parsed = run parseBString "4:spam"

// printfn "%A" parsed
