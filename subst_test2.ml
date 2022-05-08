(* #use "subst_test2.ml";; *)

#use "cc.ml"
#use "labelled_cc.ml"
#use "transformation.ml"

(* 
  FACT: Lambdas can appear in context and type,
  we might even create new functions in the inferred type!
  A new function is created when a term is substituted in a lambda term.
*)

module Source = struct
	open CC
	
	(* () : 1 means Unit : UnitType *)
	(* \Gamma = A : (1 -> 1) -> U0, 
							a : (f : 1 -> 1) -> A (\x : 1 -> f x) *)
	let con = [
		(String "a", Pi(String "f", Pi(String "x", UnitType, UnitType), App(Var(String "A"), Lambda(String "x", UnitType, App(Var(String "f"), Var(String "x"))))));
		(String "A", Pi(String "f", Pi(String "x", UnitType, UnitType), Universe 0))]

	(* e = (\f : (1 -> 1) -> a f) *)
	let e = Lambda(String "f", Pi(String "x", UnitType, UnitType), App(Var(String "a"), Var(String "f")))

	(* T = (f : 1 -> 1) -> A (\x : 1 -> f x) *)
	(* T is equivalent to (f : 1 -> 1) -> A f *)
	let t = infer_type con e
	let tn = Pi(String "f", Pi(String "x", UnitType, UnitType), App(Var(String "A"), Var(String "f")))

	let test = CC.type_check con e tn

end


(*
  Helper language LCC:
  	it must label all functions in context, expression, and inferred types.
*)
module LSource = struct
	open LCC
	let con = translate_ctx Source.con
	let e = translate Source.e
	let t = translate Source.t
	let tn = translate (CC.normalize Source.con Source.tn)
end

open Defun
let _ = LSource.e

let con = transform_lab_ctx LSource.con
let e = transform_lab LSource.con LSource.e
let t = transform_lab LSource.con LSource.t

(* let defs = merge_defs (merge_defs (def_ctx_lab LSource.con) (def_lab LSource.con LSource.e)) (def_lab LSource.con LSource.t)
let typecheck = type_check (mk_ctx defs con) e t *)
(* 
let defs2 = merge_defs (merge_defs (def_ctx_lab LSource.con) (def_lab LSource.con LSource.e)) (def_lab LSource.con LSource.tn)
let tn = transform_lab LSource.con LSource.tn
let typecheck2 = type_check (mk_ctx defs2 con) e tn *)

let _ = check_type_preservation Source.con Source.e Source.t

