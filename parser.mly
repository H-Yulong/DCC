(*
 *  Yacc grammar for the parser.  The files parser.mli and parser.ml
 *  are generated automatically from parser.mly.
 *)

%{
open Cc
open Err.Error
%}

(* ---------------------------------------------------------------------- *)
(* Preliminaries *)

%token <string Err.Error.withinfo> Var
%token <int Err.Error.withinfo> Universe
%token <Err.Error.info> UnitType

%token <Err.Error.info> COMMA
%token <Err.Error.info> COLON
%token <Err.Error.info> LAM
%token <Err.Error.info> DOT
%token <Err.Error.info> PI
%token <Err.Error.info> LPAREN
%token <Err.Error.info> RPAREN
%token <Err.Error.info> LSQUARE
%token <Err.Error.info> RSQUARE
%token <Err.Error.info> EOF

(* ---------------------------------------------------------------------- *)

%start cc_expr cc_env
%type <CC.expr> cc_expr
%type <CC.context> cc_env
%%

cc_expr:
  | CCExpr EOF                                   { $1 }

CCExpr: 
  | CCApp                                        { $1 }
  | LAM x = Var COLON t = CCExpr DOT e = CCExpr  { CC.Lambda (CC.String x.v, t, e) }
  | PI x = Var COLON t = CCExpr DOT e = CCExpr   { CC.Pi (CC.String x.v, t, e) }

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
  | CCEnv EOF { List.rev $1 }

CCEnv:
  | LSQUARE RSQUARE {[]}
  | LSQUARE CCEnvList RSQUARE {$2}

CCEnvList:
  | Var COLON CCExpr {[(CC.String $1.v, $3)]}
  | Var COLON CCExpr COMMA CCEnvList {(CC.String $1.v, $3) :: $5}

(*   *)
