open FParsec
open Bencode


let parsed = run (parseValue ()) "d3:bar4:spam3:fooi42ee"

printf "%A" parsed