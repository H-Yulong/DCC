(* #use "transformation.ml";; *)

#use "cc.ml"
#use "labelled_cc.ml"
#use "dcc.ml"

open DCC

(* [transform_var x] turns a variable in CC to a variable in DCC *)
let transform_var = function
	| CC.String s -> String s
	| CC.Gensym (s, i) -> Gensym (s, i)

(* [transform_var x] turns a variable in LCC to a variable in DCC *)
let transform_lab_var = function
	| LCC.String s -> String s
	| LCC.Gensym (s, i) -> Gensym (s, i)

(* [remove x ls] removes all occurences of x in list ls *)
let rec remove x = function
	| [] -> []
	| (y :: ys) -> if (x = y) then remove x ys else y :: (remove x ys)

let remove_dup ls = 
	let rec remove_dup2 l1 l2 =
		match l2 with
		| [] -> l1
		| (x :: xs) -> if List.mem x l1 then remove_dup2 l1 xs else remove_dup2 (x :: l1) xs
	in
	   List.rev (remove_dup2 [] ls)

(* [fv_t e] returns a list of free variables in the LCC expression e 
	fv_t stands for "free variables in a term" *)
let fv_t e = 
	let rec fv_t = function
		| LCC.Var x -> [x]
		| LCC.Universe _ | LCC.UnitType | LCC.Unit -> []
		| LCC.Pi (x, t, e) | LCC.Lambda ((x, t, e), _) -> (fv_t t) @ (remove x (fv_t e)) 
		| LCC.App (e1, e2) -> (fv_t e1) @ (fv_t e2)
	in
		remove_dup (fv_t e)

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
		remove_dup (fv_vars fvs)


(* [transform_lab ctx e] computes the defunctionalized term in DCC for e in LCC. *)
let rec transform_lab ctx = function
	| LCC.Var x -> Var (transform_lab_var x)
	| LCC.Universe i -> Universe i
	| LCC.Pi (x, t, e) -> Pi (transform_lab_var x, transform_lab ctx t, transform_lab ((x,t) :: ctx) e)
	| LCC.Lambda ((x, t, e), c) -> 
		let fvs = List.map (fun x -> Var (transform_lab_var x)) (fv ctx (LCC.Lambda ((x, t, e), c)))
		in Label(Labsym("_", c), fvs) 
	| LCC.App (e1, e2) -> Apply (transform_lab ctx e1, transform_lab ctx e2)
	| LCC.UnitType -> UnitType
	| LCC.Unit -> Unit

(* [transform_ctx ctx] computes the transformed context from LCC. *)
let rec transform_lab_ctx = function
	| [] -> []
	| (x, t) :: ctx -> (transform_lab_var x, transform_lab ctx t) :: transform_lab_ctx ctx

(* [merge_defs l1 l2] merges two definition contexts, used as a helper function in def_lab. *)
let merge_defs l1 l2 =
	let rec f buf = function
		| [] -> buf
		| (label, def) :: ls -> 
			if List.mem_assoc label buf then f buf ls
			else f ((label, def) :: buf) ls
	in f l2 (List.rev l1)

(* [def_lab ctx e] returns a list of all the function definitions appeared in the derivation tree of ctx |- e : A. *)
let rec def_lab_full ctx e ty = match e, ty with
	| LCC.Var _, _ | LCC.Universe _, _ | LCC.UnitType, _ | LCC.Unit, _ -> []
	| LCC.Pi (x, t, e), u -> merge_defs (def_lab_full ctx t u) (def_lab_full ((x, t) :: ctx) e u)
	| LCC.App (e1, e2), t -> 
		let t1 = LCC.infer_type ctx e1 in
		let t2 = LCC.infer_type ctx e2 in
			merge_defs (merge_defs (def_lab_full ctx e1 t1) (def_lab_full ctx e2 t2)) (def_lab_full ctx t (Universe 0))
	| LCC.Lambda ((x, t, e), c), LCC.Pi(_, _, te) ->
		let fvs = List.map (fun x -> (transform_lab_var x, transform_lab ctx (LCC.infer_type ctx (LCC.Var x)))) 
					(fv ctx (LCC.Lambda ((x, t, e), c))) in
		let ctx' = (x, t) :: ctx in
		let t' = transform_lab ctx t in
		let te' = transform_lab ctx' te in
		let e' = transform_lab ctx' e in
					merge_defs ((Labsym("_", c), (mk_def fvs (transform_lab_var x) t' te' e')) :: (def_lab_full ctx t (Universe 0))) (def_lab_full ctx' e te)

	| _, _ -> raise (DCC.Error "Error in transformation!")

let def_lab ctx e = def_lab_full ctx e (LCC.infer_type ctx e)

let rec def_ctx_lab = function
	| [] -> []
	| (x, t) :: ctx -> (def_lab ctx t) @ (def_ctx_lab ctx)

let check_type_preservation ctx e t = 
	let ctx' = LCC.translate_ctx ctx in
	let e' = LCC.translate e in
	let t' = LCC.translate t in
	let defs = merge_defs (merge_defs (def_lab ctx' t') (def_lab ctx' e')) (def_ctx_lab ctx')in
		type_check (mk_ctx defs (transform_lab_ctx ctx')) (transform_lab ctx' e') (transform_lab ctx' t')

let transform_full ctx e t = 
	let ctx' = LCC.translate_ctx ctx in
	let e' = LCC.translate e in
	let t' = LCC.translate t in
	let defs = merge_defs (merge_defs (def_lab ctx' t') (def_lab ctx' e')) (def_ctx_lab ctx') in
	let dcc_ctx = mk_ctx defs (transform_lab_ctx ctx') in
		(dcc_ctx, transform_lab ctx' e', transform_lab ctx' t')

let transform ctx e = 
	let t = CC.infer_type ctx e in transform_full ctx e t
		