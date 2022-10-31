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

let parse process input place = 
  let s = Js.to_string input in
  let lexbuf = Lexer.from_string s in
  let res = 
    try 
      process Lexer.main lexbuf
    with Parser.Error -> (error (Lexer.info lexbuf) ("parse error in " ^ place))
  in
    res

let cc_check_ctx env = 
  let res = 
    try 
      Buffer.clear output_buffer;
      let env = parse Parser.cc_env env "CC context"in
      Js.Unsafe.set Js.Unsafe.global "cc_status" 0;
      CC.well_formed env;
      "Context OK."
    with Exit status ->
      Js.Unsafe.set Js.Unsafe.global "cc_status" status;
      Buffer.contents output_buffer
  in
    Js.string res

let cc_infer env term =
  let res = 
    try 
      Buffer.clear output_buffer;
      let env = parse Parser.cc_env env "CC context" in
      let term = parse Parser.cc_expr term "CC term" in
      Js.Unsafe.set Js.Unsafe.global "cc_status" 0;
      CC.pprint (CC.infer_type env term)
    with Exit status ->
      Js.Unsafe.set Js.Unsafe.global "cc_status" status;
      Buffer.contents output_buffer
  in
    Js.string res

let cc_normalize env term =
  let res = 
    try 
      Buffer.clear output_buffer;
      let env = parse Parser.cc_env env "CC context" in
      let term = parse Parser.cc_expr term "CC term" in
      Js.Unsafe.set Js.Unsafe.global "cc_status" 0;
      CC.pprint (CC.normalize env term)
    with Exit status ->
      Js.Unsafe.set Js.Unsafe.global "cc_status" status;
      Buffer.contents output_buffer
  in
    Js.string res

let cc_check env term ty =
  let res = 
    try 
      Buffer.clear output_buffer;
      let env = parse Parser.cc_env env "CC context" in
      let term = parse Parser.cc_expr term "CC term" in
      let ty = parse Parser.cc_expr ty "CC type" in
      Js.Unsafe.set Js.Unsafe.global "cc_status" 0;
      CC.type_check env term ty
    with Exit status ->
      Js.Unsafe.set Js.Unsafe.global "cc_status" status;
      false
  in
    Js.bool res

let dcc_infer defs env term =
  let res = 
    try 
      Buffer.clear output_buffer;
      let defs = parse Parser.dcc_lab_env defs "DCC label context" in
      let env = parse Parser.dcc_env env "DCC term" in
      let term = parse Parser.dcc_expr term "DCC context" in
      let context = DCC.mk_ctx defs env in
      Js.Unsafe.set Js.Unsafe.global "dcc_status" 0;
      DCC.pprint (DCC.infer_type context term)
    with Exit status ->
      Js.Unsafe.set Js.Unsafe.global "dcc_status" status;
      Buffer.contents output_buffer
  in
    Js.string res

let dcc_check defs env term ty =
  let res = 
    try 
      Buffer.clear output_buffer;
      let defs = parse Parser.dcc_lab_env defs "DCC label context" in
      let env = parse Parser.dcc_env env "DCC term" in
      let term = parse Parser.dcc_expr term "DCC context" in
      let ty = parse Parser.dcc_expr ty "DCC context" in
      let context = DCC.mk_ctx defs env in
      Js.Unsafe.set Js.Unsafe.global "dcc_status" 0;
      DCC.type_check context term ty
    with Exit status ->
      Js.Unsafe.set Js.Unsafe.global "dcc_status" status;
      false
  in
    Js.bool res

let transform env term ty = 
  try
    Buffer.clear output_buffer;
    let env = parse Parser.cc_env env "CC context" in
    let term = parse Parser.cc_expr term "CC term" in
    let ty = parse Parser.cc_expr ty "CC type" in
    let (denv, de, dt) = transform_full env term ty in
    Js.Unsafe.set Js.Unsafe.global "dcc_status" 0;
    (
      Js.string (DCC.print_lab_env (List.rev denv.def)), 
      Js.string (DCC.print_env (List.rev denv.con)),
      Js.string (DCC.pprint de),
      Js.string (DCC.pprint dt),
      Js.string ""
    )
  with Exit status ->
    let msg = "Transformation error:\n" ^ (Buffer.contents output_buffer) in
    Js.Unsafe.set Js.Unsafe.global "dcc_status" status;
    (Js.string "", Js.string "", Js.string "", Js.string "", Js.string msg)

let back_transform defs env term ty = 
  try
    Buffer.clear output_buffer;
    let defs = parse Parser.dcc_lab_env defs "DCC label context" in
    let env = parse Parser.dcc_env env "DCC context" in
    let term = parse Parser.dcc_expr term "DCC term" in
    let ty = parse Parser.dcc_expr ty "DCC type" in
    let context = DCC.mk_ctx defs env in
    Js.Unsafe.set Js.Unsafe.global "cc_status" 0;
    (
      Js.string (CC.print_env (List.rev (back_transform_ctx context))),
      Js.string (CC.pprint (back_transform context term)),
      Js.string (CC.pprint (back_transform context ty)),
      Js.string ""
    )
  with Exit status ->
    let msg = "Transformation error:\n" ^ (Buffer.contents output_buffer) in
    Js.Unsafe.set Js.Unsafe.global "cc_status" status;
    (Js.string "", Js.string "", Js.string "", Js.string msg)


let () = 
  Js.Unsafe.set Js.Unsafe.global "cc_check_ctx" (Js.wrap_callback cc_check_ctx);
  Js.Unsafe.set Js.Unsafe.global "cc_infer" (Js.wrap_callback cc_infer);
  Js.Unsafe.set Js.Unsafe.global "cc_normalize" (Js.wrap_callback cc_normalize);
  Js.Unsafe.set Js.Unsafe.global "cc_check" (Js.wrap_callback cc_check);
  Js.Unsafe.set Js.Unsafe.global "dcc_infer" (Js.wrap_callback dcc_infer);
  Js.Unsafe.set Js.Unsafe.global "dcc_check" (Js.wrap_callback dcc_check);
  Js.Unsafe.set Js.Unsafe.global "transform" (Js.wrap_callback transform);
  Js.Unsafe.set Js.Unsafe.global "back_transform" (Js.wrap_callback back_transform)
