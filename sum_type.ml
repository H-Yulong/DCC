(* #use "sum_type.ml";; *)

#use "cc.ml"
#use "defun.ml"
#use "transformation.ml"

(* 
	The example we discussed in the meeting 
	tl;dr: no bugs, it works
*)

(* In Agda: 

Sum : (A : Set) -> (B : (x : A) -> Set) -> Set1
Sum A B = (K : Set) -> (k : (x : A) -> (y : B x) -> K) -> K

Let's write Sum in CC and see if its type can be correctly inferred...
*)

let bigsum = 
	let open CC in 
			Lambda(String "A", Universe 0,
			Lambda(String "B", Pi(String "x", Var (String "A"), Universe 0),
			   Pi(String "K", Universe 0,
			    Pi(String "k", Pi(String "x", Var(String "A"), 
			    	Pi(String "y", App(Var(String "B"), Var(String "x")), Var(String "K"))), Var(String "K")
				))
			))

let _ = CC.infer_type [] bigsum

(* Outoput (seem correct!): 
	Pi
	 (String "A", Universe 0,
	  Pi (String "B", Pi (String "x", Var (String "A"), Universe 0), Universe 1))
*)

(* ---------------------------------------------------- *)

(* In Agda: 

sum : (A : Set) -> (B : (x : A) -> Set) -> (x : A) -> (y : B x) -> Sum A B
sum A B x y K k = k x y

Wrtie sum and the type of sum in CC,
see if type checking works.

*)


let sum = let open CC in
		  Lambda(String "A", Universe 0,
		  Lambda(String "B", Pi(String "x", Var(String "A"), Universe 0),
		  Lambda(String "x", Var(String "A"),
		  Lambda(String "y", App(Var(String "B"), Var(String "x")),
		  Lambda(String "K", Universe 0,
		  Lambda(String "k", Pi(String "x", Var(String "A"), 
		  						Pi(String "y", App(Var(String "B"), Var(String "x")), Var(String "K"))),
		  	App(App(Var(String "k"), Var(String "x")), Var(String "y"))
		))))))

let sum_type = let open CC in 
		Pi(String "A", Universe 0,
		Pi(String "B", Pi(String "x", Var(String "A"), Universe 0),
		Pi(String "x", Var(String "A"),
		Pi(String "y", App(Var(String "B"), Var(String "x")),
			App(App(bigsum, Var(String "A")), Var(String "B"))
		))))

let sum_type_infer = CC.infer_type [] sum
let _ = CC.type_check [] sum sum_type

(* Output: true, it works! *)

(* Now we try the defunctionalization... *)

(* Compute the transformed sum and def(sum) *)
let sum_dfc = transform [] sum
let sum_def = def [] sum

(*
  Outputs:
  val sum_dfc : Defun.expr = Label (Labsym ("_", 0), [])
  
  The def(sum) is a very long list that OCaml doesn't bother to show.
*)

let con = Defun.mk_ctx sum_def []

(* Type inference, type preservation *)
let sum_type_dfc = transform [] sum_type_infer
let sum_type_dfc_infer = Defun.infer_type con sum_dfc

let _ = Defun.type_check con sum_dfc sum_type_dfc

(* Output: true! *)

