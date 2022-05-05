(* #use "equal_test.ml";; *)

#use "cc.ml"
#use "defun.ml"

open Defun

(* Eta-equivalence *)

let f1 = let open CC in Var(String "f")
let f2 = let open CC in Lambda(String "x", UnitType, App(Var (String "f"), Var(String "x")))
let f3 = let open CC in Lambda(String "y", UnitType, App(f2, Var(String "y")))
let ctx = let open CC in [(String "f", Pi(String "y", UnitType, UnitType))]


let _ = CC.equal ctx f3 f2


let f1 = (Lab "f1", mk_def [] (String "x") UnitType UnitType (Var (String "x")))
let f2' = (Lab "f2'", mk_def [] (String "z") UnitType UnitType (Var (String "z")))
let f2 = (Lab "f2", mk_def [] (String "y") UnitType UnitType (Apply(Label(Lab "f2'", []), Var(String "y"))))

let ctx = mk_ctx [f1; f2; f2'] []
let _ = equal ctx (Label(Lab "f1", [])) (Label(Lab "f2", []))
