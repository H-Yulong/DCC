(*
 *  Yacc grammar for the parser.  The files parser.mli and parser.ml
 *  are generated automatically from parser.mly.
 *)

%{
open Cc
%}

(* ---------------------------------------------------------------------- *)
(* Preliminaries *)

%token <int> Universe
%token EOF

(* ---------------------------------------------------------------------- *)

%start <CC.expr> ccexpr
%%

ccexpr:
  | e = ccexpr_sub; EOF { e }

ccexpr_sub: 
  | i = Universe {CC.Universe i}

(*   *)
