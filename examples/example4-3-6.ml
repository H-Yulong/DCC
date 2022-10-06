#use "cc.ml"
#use "dcc.ml"
#use "transformation.ml"

open CC

(* The polymorphic identity function *)

let idfun = Lambda (String "A", Universe 0, Lambda (String "x", Var (String "A"), Var (String "x")))
let t = infer_type [] idfun
let _ = normalize [] (App(App(idfun, UnitType), Unit))
let _ = type_check [] (App(App(idfun, UnitType), Unit)) UnitType;;

let (con', idfun', t') = transform_full [] idfun t
let _ = DCC.type_check con' idfun' t'
