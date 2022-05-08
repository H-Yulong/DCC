(* #use "defun.ml";; *)

(* Defunctionalized Calculus of Constructions (DCC),
   with infinite universe hierarchy and unit type.
*)

module Defun = struct 

	(* SYNTAX *)
	exception Error of string

	type variable = 
		| String of string 
		| Gensym of string * int

	type label =
		| Lab of string
		| Labsym of string * int

	type expr = 
		| Var of variable
		| Universe of int
		| Label of label * expr list
		| Apply of expr * expr
		| Pi of variable * expr * expr
		| Unit | UnitType

	(* Def body is in this form: [(xs, As)] , x, A, B, e
	   - A list of free variables and their types [(xs , As)]
	   - The bound variable x
	   - The type of the bound variable A
	   - The type of the body B
	   - The function body e
	*)
	type defItem = {
		fvs : (variable * expr) list;
		var : variable;
		dom : expr;
		cod : expr;
		expr : expr;
	}

	type context = {
		def : (label * defItem) list;
		con : (variable * expr) list;
	}

	let mk_ctx def con = {def = def; con = con}
	let mk_def fvs x s t e = {fvs = fvs; var = x; dom = s; cod = t; expr = e}

	(* Extracting from the context *)
	let rec lookup_def l ctx = List.assoc l ctx.def
	let rec lookup_var x ctx = List.assoc x ctx.con

	(* Extending a context *)
	let extend_def l d ctx = {def = (l, d) :: ctx.def; con = ctx.con}
	let extend_var x t ctx = {def = ctx.def; con = (x, t) :: ctx.con}


	(* SUBSTITUTION *)
	let refresh =
	  let k = ref 0 in
	    function
	      | String x | Gensym (x, _) -> (incr k ; Gensym (x, !k))

	(** [subst [(x1,e1); ...; (xn;en)] e] performs the given substitution of
	    expressions [e1], ..., [en] for variables [x1], ..., [xn] in expression [e]. *)
	let rec subst s = function
		| Var x -> (try List.assoc x s with Not_found -> Var x)
		| Universe k -> Universe k
		| Label (l, es) -> Label (l, List.map (subst s) es)
		| Apply (e1, e2) -> Apply (subst s e1, subst s e2)
		| Pi (x, t, e) -> 
			let x' = refresh x
			in Pi (x', subst s t, subst ((x, Var x') :: s) e)
		| Unit -> Unit
		| UnitType -> UnitType


	(* REDUCTION *)

	(** [apply_prep [(x1, A1); ...; (xn, An)] [e1; ...; en]] returns a new list
		[(x1, e1); ...; (xn, en)] and raises error if the lengths of two input lists mismatch.
		This is a helper function for evaluating applications for labels. *)
	let rec apply_prep fvs es = 
		match fvs, es with
		| [], [] -> []
		| ((x, _) :: xs , e :: es) -> (x, e) :: apply_prep xs es
		| [], _ | _, [] -> raise (Error "Wrong number of free variables applied")

	let rec normalize ctx = function
		| Var x -> Var x
		| Universe k -> Universe k
		| Label (l, es) -> Label (l, List.map (normalize ctx) es)
		| Apply (e1, e2) -> 
			let e2 = normalize ctx e2 in 
				(match normalize ctx e1 with
					| Label (l, es) ->
						(try 
							let d = lookup_def l ctx in
							let s = (d.var, e2) :: apply_prep d.fvs es in
								normalize ctx (subst s d.expr)
						with Not_found -> raise (Error "Label not found"))
					| e1 -> Apply (e1, e2))
		| Pi (x, t, e) -> Pi (x, normalize ctx t, normalize (extend_var x t ctx) e)
		| Unit -> Unit
		| UnitType -> UnitType

	(* Substitute, then normalize. Use in [equal]. *)
	let subst_normal ctx s e = normalize ctx (subst s e)

	(* EQUIVALENCE *)
	(** [equal ctx e1 e2] determines whether normalized [e1] and [e2] are equivalent up to alpha-eta equivalence. 
		For labels (l1, es1) and (l2, es2), they are equivalent if:
			- l1 points to ({xs : _}, x, Ax, Bx, e1) in Def
			- l2 points to ({ys : _}, y, Ay, By, e2) in Def
			- Ax[es1 / xs] == Ay[es2 / ys]
			- Bx[es1 / xs] == By[es2 / ys, x / y]
			- e1[es1 / xs] == e2[es2 / ys, x / y]
		Eta equivalence of (l, es) and e2:
			- l1 points to ({xs : _}, x, A, _, e) in Def
			- e[es / xs] == Apply(e2, x) in the context extended with x : A[es / xs]
	*)
	let equal ctx e1 e2 = 
		let rec equal e1 e2 =
			match e1, e2 with
				| Var x, Var y -> x = y
				| Universe k1, Universe k2 -> k1 = k2
				| Label (l1, es1), e2 -> eta_labels (l1, es1) e2
				| e1, Label(l2, es2) -> eta_labels (l2, es2) e1
				| Apply (e11, e12), Apply (e21, e22) -> equal e11 e21 && equal e12 e22
				| Pi (x, t1, e1) , Pi (y, t2, e2) -> equal t1 t2 && equal e1 (subst [(y, Var x)] e2)
				| Unit, Unit -> true
				| UnitType, UnitType -> true
				| _, _ -> false
		and eta_labels (l, es) e2 = 
			try
				let d = lookup_def l ctx in
				let sub = apply_prep d.fvs es in
					equal (subst_normal ctx sub d.expr) (normalize (extend_var d.var (subst_normal ctx sub d.dom) ctx) (Apply(e2, Var d.var)))
			with Not_found -> raise (Error "Label not found")
		in
			equal (normalize ctx e1) (normalize ctx e2)


	(* TYPE INFERENCE *)
	let check_equal ctx e1 e2 = 
		if not (equal ctx e1 e2)
		then raise (Error "Argument type does not match")

	(* This is a helper function for type-checking label terms. 

	   [check_equal_list ts fvs] takes ts = [t1, ..., tn], a list of types, 
	   and fvs = [(x1, tx1), ..., (xn, txn)], a list of variable * type pairs,
	   and checks if t1 == tx1 in ctx, t2 == tx2 in (x1, tx1) :: ctx, ... etc.
	   It returns () if everything checks; otherwise it raises an error.
	 *)
	let rec check_equal_list ctx ts fvs = 
		match ts, fvs with
			| [], [] -> ()
			| (e, t) :: ts, (x, tx) :: fvs -> 
				if not (equal ctx t tx) then raise (Error "Free-variable type does not match"); 
				check_equal_list ctx ts (List.map (fun (y, ty) -> (y, subst [(x, e)] ty)) fvs)
			| [], _ | _, [] -> raise (Error "Wrong number of free variables applied") 

	(* The type-inference algorithm.
	   For efficiency, checking of context well-formedness is defined as a separate process.
	   But, it makes sure that everytime we introduce an extended context, the new context is checked to be well-formed.
	   So, this set of rules are a bit different from standard definitions.
	*)
	let rec infer_type_fast ctx = function 
		| Var x -> lookup_var x ctx 
		| Universe k -> Universe (k + 1)
		| Label (l, es) -> infer_label ctx (l, es)
		| Apply (e1, e2) -> 
			let (x, s, t) = infer_pi ctx e1 in
			let te = infer_type_fast ctx e2 in
				check_equal ctx s te;
				subst [(x, e2)] t
		| Pi (x, t1, t2) -> 
			let k1 = infer_universe ctx t1 in 
			let k2 = infer_universe (extend_var x t1 ctx) t2 in
				Universe (max k1 k2)
		| Unit -> UnitType
		| UnitType -> Universe 0

	and infer_universe ctx e =
		let t = infer_type_fast ctx e in
		match normalize ctx t with
			| Universe k -> k
			| Var _ | Label _ | Apply _ | Pi _ | Unit | UnitType -> raise (Error "Not a universe")

	and infer_pi ctx e = 
		let t = infer_type_fast ctx e in
		match normalize ctx t with
			| Pi (x, s, t) -> (x, s, t)
			| Var _ | Universe _ | Label _ | Apply _ | Unit | UnitType -> raise (Error "Not a pi-type")

	and infer_label ctx (l, es) =
		try  
			let d = lookup_def l ctx in
			let ts = List.map (fun e -> (e, infer_type_fast ctx e)) es in
				check_equal_list ctx ts d.fvs;
				let sub = apply_prep d.fvs es in
					Pi (d.var, (subst sub d.dom), (subst sub d.cod))
		with
			| Not_found -> raise (Error "Label not found")
			| Error msg -> raise (Error msg)

	(* [infer_fvs [(x1, t1), ..., (xn, tn)] checks if t1, ..., tn are types under context ctx.
	   It returns () if everything is checked.
	*)
	let rec infer_fvs ctx = function
		| [] -> ()
		| (x, t) :: fvs -> 
			let _ = infer_universe ctx t
			in infer_fvs (extend_var x t ctx) fvs

	let rec well_formed_def = function
		| [] -> ()
		| (l, d) :: def -> 
			let _ = infer_fvs (mk_ctx def []) d.fvs in
			let fv_con = List.rev d.fvs in
			let fv_con' = (d.var, d.dom) :: fv_con in
			let _ = infer_universe (mk_ctx def fv_con) d.dom in
			let _ = infer_universe (mk_ctx def fv_con') d.cod in
			let te = infer_type_fast (mk_ctx def fv_con') d.expr in
				check_equal (mk_ctx def fv_con') te d.cod;
				well_formed_def def

	let well_formed ctx =
		let _ = well_formed_def ctx.def in
		let rec wf = function
			| [] -> ()
			| (x, t) :: con -> let _ = infer_universe (mk_ctx ctx.def con) t in wf con
		in
			wf ctx.con

	let infer_type ctx e = 
		try
			let _ = well_formed ctx in 
				infer_type_fast ctx e
		with Error msg -> raise (Error msg)
		
	(* TYPE CHECKING *)
	(* Infer type, check if the given type expression equals to the inferred type.*)
	let type_check ctx e t = 
	    try 
	      let _ = infer_universe ctx t in
	      let te = infer_type ctx e in
	      let t' = normalize ctx t in
	        equal ctx te t'
	    with Error msg -> raise (Error msg)
end;;