(* The lexical analyzer. *)

{
open Parser
open Err.Error

exception Error of string

let str_tail str = String.sub str 1 (String.length str - 1)

let lineno   = ref 1
and start    = ref 0

and filename = ref ""
and startLex = ref dummyinfo

(* To handle glyphs *)
let curr_offset = ref 0

let create inFile stream =
  if not (Filename.is_implicit inFile) then filename := inFile
  else filename := Filename.concat (Sys.getcwd()) inFile;
  lineno := 1; start := 0; Lexing.from_channel stream

let from_string s =
  filename := ""; lineno := 1; start := 0;
  Lexing.from_string s

let newline lexbuf = incr lineno; curr_offset := 0; start := (Lexing.lexeme_start lexbuf)

let info ?(offset = 0) lexbuf =
  let r = !curr_offset - !start in
  curr_offset := !curr_offset - offset;
  createInfo (!filename)
    (!lineno) (Lexing.lexeme_start lexbuf + r)
    (!lineno) (Lexing.lexeme_end lexbuf + r - offset - 1)

let text = Lexing.lexeme

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
| universe as u                     { Universe {i=info lexbuf;v=(int_of_string (str_tail u))}  }
| "\\Pi"                            { PI (info lexbuf) }
| "Pi"                              { PI (info lexbuf) }
| "\206\160"                        { PI (info lexbuf) }
| "\\"                              { LAM (info lexbuf) }
| "\206\187"                        { LAM (info lexbuf) }
| "\\lambda"                        { LAM (info lexbuf) }
| "\\lam"                           { LAM (info lexbuf) }
| "Unit"                            { UnitType (info lexbuf) }
| "->"                              { ARROW (info lexbuf) }
| "\226\134\146"                    { ARROW (info lexbuf) }
| "."                               { DOT (info lexbuf) }
| ":"                               { COLON (info lexbuf) } 
| ";"                               { SEMICOLON (info lexbuf) } 
| "("                               { LPAREN (info lexbuf) }
| ")"                               { RPAREN (info lexbuf) }
| "["                               { LSQUARE (info lexbuf) }
| "]"                               { RSQUARE (info lexbuf) }
| "{"                               { LBRACE (info lexbuf) }
| "}"                               { RBRACE (info lexbuf) }
| ","                               { COMMA (info lexbuf) }
| lident as var                     { Var {i=info lexbuf;v=var} } 
| uident as var                     { Var {i=info lexbuf;v=var} }
| eof                               { EOF (info lexbuf) }
| _                                 { error (info lexbuf) "Illegal character found" }

(*  *)