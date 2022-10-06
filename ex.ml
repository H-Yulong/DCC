(* #use "ex.ml";; *)
(* An example of two "equal" things with wrong types *)

#use "cc.ml"

open CC

let con = [
	(String "x", App(Var(String "B"), Var(String "f")));
	(String "B", Pi(String "f", Pi(String "x", Var(String "A"), Var(String "A")), Universe 0));
	(String "f", Pi(String "x", Var(String "A"), Var(String "A")));
	(String "A", Universe 0);
]

(* env |- x : A -> A *)
let e1 = Var(String "f")
(* e2 is ill-typed *)
let e2 = Lambda(String "x", UnitType, App(Var(String "f"), Var(String "x")))

(* In untyped beta-eta-equality, e1 = e2 *)
let _ = equal e1 e2
let _ = equal (App(Var(String "B"), e1)) (App(Var(String "B"), e2))
(* B: (f : A -> A) -> U0 *)

(* But you cannot use the conversion rule *)
let _ = type_check con (Var(String "x")) (App(Var(String "B"), e1))
let _ = type_check con (Var(String "x")) (App(Var(String "B"), e2))

(* The final line should throw some error like "type mismatch" *)

