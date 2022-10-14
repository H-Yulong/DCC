open Cc.CC
open Js_of_ocaml
open Err

let cc_parse s =
  let s = Js.to_string s in
  let lexbuf = Lexing.from_string s in
  let ast = Parser.cc_expr Lexer.main lexbuf in
    ast

let cc_parse_env s = 
  let s = Js.to_string s in
  let lexbuf = Lexing.from_string s in
  let ast = Parser.cc_env Lexer.main lexbuf in
    ast

let cc_infer env term =
  let env = cc_parse_env env in
  let term = cc_parse term in
    Js.string (pprint (infer_type env term))

let cc_check env term ty =
  let env = cc_parse_env env in
  let term = cc_parse term in
  let ty = cc_parse ty in
    Js.bool (type_check env term ty)

let () = Js.Unsafe.set Js.Unsafe.global "cc_infer" (Js.wrap_callback cc_infer)
let () = Js.Unsafe.set Js.Unsafe.global "cc_check" (Js.wrap_callback cc_check)


