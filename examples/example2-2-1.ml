#use "cc.ml"
open CC

(* The polymorphic identity function *)

let idfun = Lambda (String "A", Universe 0, Lambda (String "x", Var (String "A"), Var (String "x")))
let _ = infer_type [] (App(App(idfun, UnitType), Unit))
let _ = normalize [] (App(App(idfun, UnitType), Unit))
let _ = type_check [] (App(App(idfun, UnitType), Unit)) UnitType;;