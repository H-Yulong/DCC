(* #use "transformation_examples.ml";; *)

#use "cc.ml"
#use "defun.ml"
#use "transformation.ml"

(* Computing free variables *)

let idfun = let open CC in Lambda (String "A", Universe 0, Lambda (String "x", Var (String "A"), Var (String "x")))

(*
let f = let open CC in Lambda (String "x", Universe 0, Var (String "y"))
let con = let open CC in [(String "y", Var (String "A")); (String "A", Universe 0)]

let tf = CC.infer_type con f

let dc = transform con f
let dtc = transform con tf
let deff = def con f

let dcon = transform_ctx con

let _ = Defun.infer_type (Defun.mk_ctx deff dcon) dc
let _ = Defun.type_check (Defun.mk_ctx deff dcon) dc dtc
*)

(*
let idfun = let open CC in Lambda (String "A", Universe 0, Lambda (String "x", Var (String "A"), Var (String "x")))
let id = transform [] idfun
let defid = def [] idfun
let idtype = transform [] (CC.infer_type [] idfun)

let _ = DCC.infer_type (mk_ctx defid []) id
let _ = DCC.type_check (mk_ctx defid []) id idtype
*)


let compose =
	let open CC in
		Lambda (String "A", Universe 0,
		Lambda (String "B", Pi (String "x", Var (String "A"), Universe 0),
		Lambda (String "C", Pi (String "x", Var (String "A"), Pi (String "y", App (Var (String "B"), Var (String "x")), Universe 0)),
		Lambda (String "g", Pi (String "x", Var (String "A"), Pi (String "y", App (Var (String "B"), Var (String "x")), App (App (Var (String "C"), Var (String "x")), Var (String "y")))),
		Lambda (String "f", Pi (String "x", Var (String "A"), App (Var (String "B"), Var (String "x"))),
		Lambda (String "x", Var (String "A"), 
			App (App (Var (String "g"), Var (String "x")), App (Var (String "f"), Var (String "x")))
	))))))


let com_type = CC.infer_type [] compose
let compose_dfc = transform [] compose
let compose_dfc_type1 = transform [] com_type
let compose_def = def [] compose

let compose_dfc_type2 = Defun.infer_type (Defun.mk_ctx compose_def []) compose_dfc
let _ = Defun.type_check (Defun.mk_ctx compose_def []) compose_dfc compose_dfc_type1
(**)

