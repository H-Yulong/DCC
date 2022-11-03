(*
 *  Yacc grammar for the parser.  The files parser.mli and parser.ml
 *  are generated automatically from parser.mly.
 *)

%{
open Cc
open Dcc
open Err.Error
%}

(* ---------------------------------------------------------------------- *)
(* Preliminaries *)

%token <string Err.Error.withinfo> Var
%token <int Err.Error.withinfo> Universe
%token <Err.Error.info> UnitType

%token <Err.Error.info> ARROW
%token <Err.Error.info> COMMA
%token <Err.Error.info> COLON
%token <Err.Error.info> AT
%token <Err.Error.info> LAM
%token <Err.Error.info> DOT
%token <Err.Error.info> PI
%token <Err.Error.info> LPAREN
%token <Err.Error.info> RPAREN
%token <Err.Error.info> LSQUARE
%token <Err.Error.info> RSQUARE
%token <Err.Error.info> LBRACE
%token <Err.Error.info> RBRACE
%token <Err.Error.info> EOF

(* ---------------------------------------------------------------------- *)

%start cc_expr cc_env dcc_expr dcc_env dcc_lab_env
%type <CC.expr> cc_expr
%type <CC.context> cc_env
%type <DCC.expr> dcc_expr
%type <(DCC.variable * DCC.expr) list> dcc_env
%type <(DCC.label * DCC.defItem) list> dcc_lab_env
%%

cc_expr:
  | CCExpr EOF                                   { $1 }

CCExpr: 
  | CCApp                                        { $1 }
  | LAM Var COLON CCExpr DOT CCExpr              { CC.Lambda (CC.String $2.v, $4, $6) }
  | PI Var COLON CCExpr DOT CCExpr               { CC.Pi (CC.String $2.v, $4, $6) }

CCApp:
  | CCAtomic                                     { $1 }
  | CCApp CCAtomic                               { CC.App ($1, $2) }

CCAtomic: 
  | LPAREN CCExpr RPAREN                         { $2 }
  | LPAREN RPAREN                                { CC.Unit }
  | UnitType                                     { CC.UnitType }
  | Universe                                     { CC.Universe $1.v }
  | Var                                          { CC.Var (CC.String $1.v) }

cc_env:
  | CCEnv EOF                                    { List.rev $1 }

CCEnv:
  | LSQUARE RSQUARE                              { [] }
  | LSQUARE CCEnvList RSQUARE                    { $2 }

CCEnvList:
  | Var COLON CCExpr                             { [(CC.String $1.v, $3)] }
  | Var COLON CCExpr COMMA CCEnvList             { (CC.String $1.v, $3) :: $5 }

dcc_expr:
  | DCCExpr EOF                                  { $1 }

DCCExpr:
  | DCCApp                                       { $1 }
  | PI Var COLON DCCExpr DOT DCCExpr             { DCC.Pi (DCC.String $2.v, $4, $6) }

DCCExprList:
  | DCCExpr                                      { [$1] }
  | DCCExpr COMMA DCCExprList                    { $1 :: $3 }

DCCLab:
  | Var LBRACE RBRACE                            { DCC.Label (DCC.Lab $1.v, []) }
  | Var LBRACE DCCExprList RBRACE                { DCC.Label (DCC.Lab $1.v, $3) }

DCCApp:
  | DCCAtomic                                    { $1 }
  | DCCApp AT DCCAtomic                          { DCC.Apply ($1, $3) }

DCCAtomic: 
  | LPAREN DCCExpr RPAREN                        { $2 }
  | LPAREN RPAREN                                { DCC.Unit }
  | DCCLab                                       { $1 }
  | UnitType                                     { DCC.UnitType }
  | Universe                                     { DCC.Universe $1.v }
  | Var                                          { DCC.Var (DCC.String $1.v) }

dcc_env:
  | DCCEnv EOF                                   { List.rev $1 }

DCCEnv:
  | LSQUARE RSQUARE                              { [] }
  | LSQUARE DCCEnvList RSQUARE                   { $2 }

DCCEnvList:
  | Var COLON DCCExpr                            { [(DCC.String $1.v, $3)] }
  | Var COLON DCCExpr COMMA DCCEnvList           { (DCC.String $1.v, $3) :: $5 }

dcc_lab_env:
  | DCCLabEnv EOF                                { List.rev $1 }

DCCLabEnv:
  | LSQUARE RSQUARE                              { [] }
  | LSQUARE DCCLabEnvList RSQUARE                { $2 }

DCCLabEnvList:
  | Var LPAREN DCCDefItem RPAREN                               
  { [(DCC.Lab $1.v, $3)] }
  | Var LPAREN DCCDefItem RPAREN COMMA DCCLabEnvList
  { (DCC.Lab $1.v, $3) :: $6 }

DCCDefItem:
  | LBRACE RBRACE COMMA Var COLON DCCExpr ARROW DCCExpr COLON DCCExpr                   
  { DCC.mk_def [] (DCC.String $4.v) $6 $10 $8 }
  | LBRACE DCCEnvList RBRACE COMMA Var COLON DCCExpr ARROW DCCExpr COLON DCCExpr 
  { DCC.mk_def $2 (DCC.String $5.v) $7 $11 $9 }
