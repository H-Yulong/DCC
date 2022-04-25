(* #use "cc_examples.ml";; *)

#use "cc.ml"
open CC


(* CC EXAMPLES *)

let idfun = Lambda (String "A", Universe 0, Lambda (String "x", Var (String "A"), Var (String "x")))
let _ = infer_type [] (App(App(idfun, UnitType), Unit))
let _ = normalize [] (App(App(idfun, UnitType), Unit))
let _ = type_check [] (App(App(idfun, UnitType), Unit)) UnitType;;

let f = Lambda (String "x", Universe 0, Lambda (String "x", Var (String "x"), Var (String "x")))
let t = infer_type [] f
let _ = normalize [] (App(App (f, UnitType), Unit))
