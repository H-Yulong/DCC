(* #use "dcc_examples.ml";; *)

#use "defun.ml"

(* DCC EXAMPLES *)

open Defun

let id2 = (Lab "Id2", mk_def [(String "A", Universe 0)] (String "x") (Var (String "A")) (Var (String "A")) (Var (String "x")))
let id1 = (Lab "Id1", mk_def [] (String "A") (Universe 0) (Pi (String "x", Var (String "A"), Var (String "A"))) (Label(Lab "Id2", [Var (String "A")])))
let ctx = mk_ctx [id1; id2] []

let t = (Pi (String "B", Universe 0, (Pi (String "y", Var (String "B"), Var (String "B")))))

let _ = infer_type ctx (Label(Lab "Id1", []))
let _ = normalize ctx (Apply((Apply(Label(Lab "Id1", []), UnitType)), Unit))
let _ = type_check ctx (Label(Lab "Id1", [])) t

(*
let d = mk_def [(String "A", Universe 0); (String "y", Var (String "A"))] (String "x") (Universe 0) (Var (String "A")) (Var (String "y"))

let l = Label (Lab "D", [UnitType; Unit])
let ctx2 = mk_ctx [(Lab "D", d)] []

let _ = infer_label ctx2 (Lab "D", [UnitType; Unit])
*)
