(* #use "subst_test.ml";; *)

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

	(* e = a (\x : 1 -> ()) *)
	let e =
		App(Var(String "a"), Lambda(String "x", UnitType, Unit))

	(* T = A (\x : 1 -> (\x : 1 -> ()) x) *)
	(* T normalizes to A (\x : 1 -> ()) *)
	let t = infer_type con e
end

let _ = check_type_preservation Source.con Source.e Source.t

