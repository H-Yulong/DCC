(* #use "examples/subst_test.ml";; *)

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

let _ = check_type_preservation Source.con Source.e Source.t

(* open CC

let e1 = App(Var(String "A"), Lambda(String "x", UnitType, App(Var(String "f"), Var(String "x"))))
let e2 = App(Var(String "A"), Var(String "f"))

let con' = [
		(String "f", Pi(String "x", UnitType, UnitType));
		(String "a", Pi(String "f", Pi(String "x", UnitType, UnitType), App(Var(String "A"), Lambda(String "x", UnitType, App(Var(String "f"), Var(String "x"))))));
		(String "A", Pi(String "f", Pi(String "x", UnitType, UnitType), Universe 0))]

let _ = check_equal con' e1 e2 *)
