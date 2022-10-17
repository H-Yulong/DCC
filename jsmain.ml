open Format
open Cc
open Dcc
open Transformation
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
      CC.pprint (CC.infer_type env term)
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
      CC.type_check env term ty
    with Exit status ->
      Js.Unsafe.set Js.Unsafe.global "fomega_status" status;
      false
  in
    Js.bool res

let dcc_infer defs env term =
  let res = 
    try 
      Buffer.clear output_buffer;
      let defs = parse Parser.dcc_lab_env defs in
      let env = parse Parser.dcc_env env in
      let term = parse Parser.dcc_expr term in
      let context = DCC.mk_ctx defs env in
      Js.Unsafe.set Js.Unsafe.global "fomega_status" 0;
      DCC.pprint (DCC.infer_type context term)
    with Exit status ->
      Js.Unsafe.set Js.Unsafe.global "fomega_status" status;
      Buffer.contents output_buffer
  in
    Js.string res

let dcc_check defs env term ty =
  let res = 
    try 
      Buffer.clear output_buffer;
      let defs = parse Parser.dcc_lab_env defs in
      let env = parse Parser.dcc_env env in
      let term = parse Parser.dcc_expr term in
      let ty = parse Parser.dcc_expr ty in
      let context = DCC.mk_ctx defs env in
      Js.Unsafe.set Js.Unsafe.global "fomega_status" 0;
      DCC.type_check context term ty
    with Exit status ->
      Js.Unsafe.set Js.Unsafe.global "fomega_status" status;
      false
  in
    Js.bool res

let transform env term ty = 
  let env = parse Parser.cc_env env in
  let term = parse Parser.cc_expr term in
  let ty = parse Parser.cc_expr ty in
  let (denv, de, dt) = transform_full env term ty in
  (
    Js.string (DCC.print_lab_env (List.rev denv.def)), 
    Js.string (DCC.print_env (List.rev denv.con)),
    Js.string (DCC.pprint de),
    Js.string (DCC.pprint dt)
  )


let () = 
  Js.Unsafe.set Js.Unsafe.global "cc_infer" (Js.wrap_callback cc_infer);
  Js.Unsafe.set Js.Unsafe.global "cc_check" (Js.wrap_callback cc_check);
  Js.Unsafe.set Js.Unsafe.global "dcc_infer" (Js.wrap_callback dcc_infer);
  Js.Unsafe.set Js.Unsafe.global "dcc_check" (Js.wrap_callback dcc_check);
  Js.Unsafe.set Js.Unsafe.global "transform" (Js.wrap_callback transform)
