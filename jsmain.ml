open Format
open Cc.CC
open Err.Error
open Js_of_ocaml

(* Set up output buffer and formamter *)
let output_buffer = Buffer.create 100

let print = Buffer.add_substring output_buffer
let flush () = ()

let () = set_formatter_output_functions print flush

(* Function definitions *)

let parse process input = 
  let s = Js.to_string input in
  let lexbuf = Lexing.from_string s in
  let res = 
    try 
      process Lexer.main lexbuf
    with Parser.Error -> (error (Lexer.info lexbuf) "parse error")
  in
    res
    
let cc_infer env term =
  let res = 
    try 
      Buffer.clear output_buffer;
      let env = parse Parser.cc_env env in
      let term = parse Parser.cc_expr term in
      Js.Unsafe.set Js.Unsafe.global "fomega_status" 0;
      pprint (infer_type env term)
    with Exit status ->
      Js.Unsafe.set Js.Unsafe.global "fomega_status" status;
      Buffer.contents output_buffer
  in
    Js.string res

let cc_check env term ty =
  let res = 
    try 
      Buffer.clear output_buffer;
      let env = parse Parser.cc_env env in
      let term = parse Parser.cc_expr term in
      let ty = parse Parser.cc_expr ty in
      Js.Unsafe.set Js.Unsafe.global "fomega_status" 0;
      type_check env term ty
    with Exit status ->
      Js.Unsafe.set Js.Unsafe.global "fomega_status" status;
      false
  in
    Js.bool res

let () = Js.Unsafe.set Js.Unsafe.global "cc_infer" (Js.wrap_callback cc_infer)
let () = Js.Unsafe.set Js.Unsafe.global "cc_check" (Js.wrap_callback cc_check)

