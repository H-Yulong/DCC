(*
 *  Yacc grammar for the parser.  The files parser.mli and parser.ml
 *  are generated automatically from parser.mly.
 *)

%{
open Cc
%}

(* ---------------------------------------------------------------------- *)
(* Preliminaries *)

%token <string> Var
%token <int> Universe
%token UnitType

%token COMMA
%token COLON
%token LAM
%token DOT
%token PI
%token LPAREN
%token RPAREN
%token LSQUARE
%token RSQUARE
%token EOF

(* ---------------------------------------------------------------------- *)

%start cc_expr cc_env
%type <CC.expr> cc_expr
%type <CC.context> cc_env
%%

cc_expr:
  | CCExpr EOF                                   { $1 }

CCExpr: 
  | CCApp                                        { $1 }
  | LAM x = Var COLON t = CCExpr DOT e = CCExpr  { CC.Lambda (CC.String x, t, e) }
  | PI x = Var COLON t = CCExpr DOT e = CCExpr   { CC.Pi (CC.String x, t, e) }

CCApp:
  | CCAtomic                                     { $1 }
  | CCApp CCAtomic                               { CC.App ($1, $2) }

CCAtomic: 
  | LPAREN CCExpr RPAREN                         { $2 }
  | LPAREN RPAREN                                { CC.Unit }
  | UnitType                                     { CC.UnitType }
  | Universe                                     { CC.Universe $1 }
  | Var                                          { CC.Var (CC.String $1) }

cc_env:
  | CCEnv EOF { List.rev $1 }

CCEnv:
  | LSQUARE RSQUARE {[]}
  | LSQUARE CCEnvList RSQUARE {$2}

CCEnvList:
  | Var COLON CCExpr {[(CC.String $1, $3)]}
  | Var COLON CCExpr COMMA CCEnvList {(CC.String $1, $3) :: $5}

(*   *)
