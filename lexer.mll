(* The lexical analyzer. *)

{
open Parser
open Err.Error

let str_tail str = String.sub str 1 (String.length str - 1)
}

let whitespace = [' ' '	']
let idchar =  ['A'-'Z' 'a'-'z' '_' '0'-'9' '\'']
let lident = ['a'-'z' '_'] idchar*
let uident = ['A'-'Z'] idchar*

let digit = ['0'-'9']
let firstdigit = ['1'-'9']
let index = firstdigit digit*

let subdigit = "\226\130"['\128'-'\137']
let firstsubdigit = "\226\130"['\129'-'\137']
let subindex = firstsubdigit subdigit*


let universe = ['U'] digit*


(* The main body of the lexical analyzer *)

rule main = parse
  whitespace+                       { main lexbuf }
| whitespace*("\r")?"\n"            { main lexbuf }
| universe as u                     { Parser.Universe (int_of_string (str_tail u)) }
| "Pi"                              { Parser.PI }
| "\\"                              { Parser.LAM }
| "."                               { Parser.DOT }
| ":"                               { Parser.COLON } 
| "("                               { Parser.LPAREN }
| ")"                               { Parser.RPAREN }
| "["                               { Parser.LSQUARE }
| "]"                               { Parser.RSQUARE }
| ","                               { Parser.COMMA }
| "Unit"                            { Parser.UnitType }
| lident as var                     { Parser.Var (var) } 
| eof { EOF }

(*  *)