open FParsec
open Bencode


let parsed = run (parseValue ()) "li-234232e,i231ee"

printf "%A" parsed