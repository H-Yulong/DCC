(* #use "fin.ml";; *)

#use "cc.ml"
#use "labelled_cc.ml"
#use "dcc.ml"
#use "transformation.ml"

module Source = struct
	open CC

	let con = [
			(String "f", App(Var(String "Fin"), Var(String "x")));
			(String "x", Var(String "N"));
			(* Finite sets *)
			(String "fzero", Pi(String "x", Var(String "N"), App(Var(String "Fin"), App(Var(String "suc"), Var(String "x")))));
			(String "Fin", Pi(String "x", Var(String "N"), Universe 0));
			(* Natural numbers *)
			(String "suc", Pi(String "x", Var(String "N"), Var(String "N")));
			(String "z", Var(String "N"));
			(String "N", Universe 0)
		]

	let e = Lambda(String "y", Var(String "N"), Var(String "f"))

	let t = infer_type con e
end

module LS = struct
	open LCC
	let con = translate_ctx Source.con
	let e = translate Source.e
	let t = translate Source.t
	let defs = merge_defs (merge_defs (def_lab con t) (def_lab con e)) (def_ctx_lab con)
	let dcon = DCC.mk_ctx defs (transform_lab_ctx con)
end

open DCC
let e_bar = [
	Var(String "N"); Var(String "Fin"); 
	Apply(Var(String "suc"), Var(String "z"));
	Apply(Var(String "fzero"), Var(String "z"))
	]
let exp = Label(Labsym("_", 0), e_bar)

