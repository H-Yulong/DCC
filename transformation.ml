(* #use "transformation.ml";; *)

#use "cc.ml"
#use "labelled_cc.ml"
#use "defun.ml"

open Defun

(* [transform_var x] turns a variable in CC to a variable in DCC *)
let transform_var = function
	| CC.String s -> String s
	| CC.Gensym (s, i) -> Gensym (s, i)
	| CC.Dummy -> Dummy

(* [transform_var x] turns a variable in LCC to a variable in DCC *)
let transform_lab_var = function
	| LCC.String s -> String s
	| LCC.Gensym (s, i) -> Gensym (s, i)
	| LCC.Dummy -> Dummy

(* [remove x ls] removes all occurences of x in list ls *)
let rec remove x = function
	| [] -> []
	| (y :: ys) -> if (x = y) then remove x ys else y :: (remove x ys)

(* [fv_t e] returns a list of free variables in the LCC expression e 
	fv_t stands for "free variables in a term" *)
let fv_t e = 
	let rec fv_t = function
		| LCC.Var x -> [x]
		| LCC.Universe _ | LCC.UnitType | LCC.Unit -> []
		| LCC.Pi (x, t, e) | LCC.Lambda ((x, t, e), _) -> (fv_t t) @ (remove x (fv_t e)) 
		| LCC.App (e1, e2) -> (fv_t e1) @ (fv_t e2)
	in
		List.sort_uniq compare (fv_t e)

(* [fv e] returns the list of free variables needed to type a LCC expression e 
	This is a helper function for transforming lambda terms.
*)
let rec fv ctx e =
	let fvs = fv_t e in
	let rec fv_vars = function
		| [] -> fvs
		| x :: xs -> 
			(fv ctx (LCC.infer_type ctx (LCC.Var x))) 
			@ (fv_vars xs) 
			@ fvs
	in
		List.sort_uniq compare (fv_vars fvs)


(* [transform_lab ctx e] computes the defunctionalized term in DCC for e in LCC. *)
let rec transform_lab ctx = function
	| LCC.Var x -> Var (transform_lab_var x)
	| LCC.Universe i -> Universe i
	| LCC.Pi (x, t, e) -> Pi (transform_lab_var x, transform_lab ctx t, transform_lab ctx e)
	| LCC.Lambda ((x, t, e), c) -> 
		let fvs = List.map (fun x -> Var (transform_lab_var x)) (fv ctx (LCC.Lambda ((x, t, e), c)))
		in Label(Labsym("_", c), fvs) 
	| LCC.App (e1, e2) -> Apply (transform_lab ctx e1, transform_lab ctx e2)
	| LCC.UnitType -> UnitType
	| LCC.Unit -> Unit

let transform ctx e = transform_lab (LCC.translate_ctx ctx) (LCC.translate e)

(* [transform_ctx ctx] computes the transformed context from CC.
   For every x : A in ctx, it normalizes A before transforming A. *)
let rec transform_ctx = function
	| [] -> []
	| (x, t) :: ctx -> (transform_var x, transform ctx (CC.normalize ctx t)) :: transform_ctx ctx

(* [transform_ctx ctx] computes the transformed context from LCC.
   For every x : A in ctx, it normalizes A before transforming A. *)
let rec transform_lab_ctx = function
	| [] -> []
	| (x, t) :: ctx -> (transform_lab_var x, transform_lab ctx (LCC.normalize ctx t)) :: transform_lab_ctx ctx

let rec def_lab ctx = function
	| LCC.Var _ | LCC.Universe _ | LCC.UnitType | LCC.Unit -> []
	| LCC.Pi (x, t, e) -> (def_lab ctx t) @ (def_lab ((x, t) :: ctx) e)
	| LCC.App (e1, e2) -> (def_lab ctx e1) @ (def_lab ctx e2)
	| LCC.Lambda ((x, t, e), c) -> 
		let fvs = List.map (fun x -> (transform_lab_var x, transform_lab ctx (LCC.infer_type ctx (LCC.Var x)))) 
					(fv ctx (LCC.Lambda ((x, t, e), c))) in
		let ctx' = (x, t) :: ctx in
		let t' = transform_lab ctx t in 
		let te = LCC.infer_type ctx' e in
		let te' = transform_lab ctx' te in
		let e' = transform_lab ctx' e in
			(Labsym("_", c), (mk_def fvs (transform_lab_var x) t' te' e')) :: (def_lab ctx t) @ (def_lab ctx' e)

(* [def ctx e] computes all the lambda definitions in e. *)
let def ctx e = def_lab (LCC.translate_ctx ctx) (LCC.translate e)
