open Cc.CC

let s = "U1050"

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.ccexpr Lexer.main lexbuf in
  ast

let () = print_endline (pprint (parse s))
let () = print_endline "Hello!"


