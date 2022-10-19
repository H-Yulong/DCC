(* #use "labelled_cc.ml";; *)
(* Calculus of Constructions, with infinite universe hierarchy
   and unit type, and every lambda is labelled uniquely.

   !!! Should only be used as an internal representation. !!!
*)

open Cc
open Err.Error

module LCC = struct

  (* SYNTAX *)

  (* Gensym for renaming variables; Dummy for pretty printing. *)
  type variable =
    | String of string
    | Gensym of string * int

  (* Abstraction (x, A, t) means x:A and x is free in t *)
  type expr =
    | Var of variable
    | Universe of int
    | Pi of abstraction
    | Lambda of abstraction * int
    | App of expr * expr
    | UnitType | Unit
  and abstraction = variable * expr * expr

  type context = (variable * expr) list

  (** [lookup_ty x ctx] returns the type of [x] in context [ctx]. *)
  let lookup_ty x ctx = List.assoc x ctx

  (** [extend x t ctx] returns [ctx] extended with variable [x] of type [t]. *)
  let extend x t ctx = (x, t) :: ctx


  (* SUBSTITUTION *)

  (** [refresh x] generates a fresh variable name whose preferred form is [x]. *)
  let refresh =
    let k = ref 0 in
      function
        | String x | Gensym (x, _) -> (incr k ; Gensym (x, !k))

  let label_counter = ref (-1)

  let refresh_label () = (incr label_counter); !label_counter 

  let init () = label_counter := -1

  (** [subst [(x1,e1); ...; (xn;en)] e] performs the given substitution of
      expressions [e1], ..., [en] for variables [x1], ..., [xn] in expression [e]. *)
  let rec subst s = function
    | Var x -> (try List.assoc x s with Not_found -> Var x)
    | Universe k -> Universe k
    | Pi a -> Pi (subst_abstraction s a)
    | Lambda (a, i) -> Lambda ((subst_abstraction s a), refresh_label ())
    | App (e1, e2) -> App (subst s e1, subst s e2)
    | UnitType -> UnitType
    | Unit -> Unit

  and subst_abstraction s (x, t, e) =
    let x' = refresh x in
      (x', subst s t, subst ((x, Var x') :: s) e)


  (* REDUCTION *)

  (** [normalize ctx e] normalizes the given expression [e] in context [ctx]. It removes
      all redexes and it unfolds all definitions. It performs normalization under binders.  *)
  let rec normalize ctx = function
    | Var x -> Var x
    | App (e1, e2) ->
      let e2 = normalize ctx e2 in
        (match normalize ctx e1 with
          | Lambda ((x, _, e1'), _) -> normalize ctx (subst [(x,e2)] e1')
          | e1 -> App (e1, e2))
    | Universe k -> Universe k
    | Pi a -> Pi (normalize_abstraction ctx a)
    | Lambda (a, i) -> Lambda ((normalize_abstraction ctx a), i)
    | UnitType -> UnitType
    | Unit -> Unit

  and normalize_abstraction ctx (x, t, e) =
    let t = normalize ctx t in
      (x, t, normalize (extend x t ctx) e)


  (* EQUIVALENCE *)

  (** [equal ctx e1 e2] determines whether normalized [e1] and [e2] are equivalent up to renaming
      of bound variables. *)
  let equal ctx e1 e2 =
    let rec equal e1 e2 =
      match e1, e2 with
        | Var x1, Var x2 -> x1 = x2
        | App (e11, e12), App (e21, e22) -> equal e11 e21 && equal e12 e22
        | Universe k1, Universe k2 -> k1 = k2
        | Pi a1, Pi a2 -> equal_abstraction a1 a2
        | Lambda (a1, _), Lambda (a2, _) -> equal_abstraction a1 a2
        | UnitType, UnitType -> true
        | Unit, Unit -> true
        | (Var _ | App _ | Universe _ | Pi _ | Lambda _ | UnitType | Unit), _ -> false
    and equal_abstraction (x, t1, e1) (y, t2, e2) =
      equal t1 t2 && (equal e1 (subst [(y, Var x)] e2))
    in
      equal (normalize ctx e1) (normalize ctx e2)


  (* TYPE INFERENCE *)

  (** [infer_type ctx e] infers the type of expression [e] in context [ctx].  *)
  let rec infer_type ctx = function
    | Var x -> 
      (try 
         well_formed ctx; lookup_ty x ctx
         with 
            | Not_found -> (err "Undefined variable"))
    | Universe k -> well_formed ctx; Universe (k + 1)
    | Pi (x, t1, t2) ->
      let k1 = infer_universe ctx t1 in
      let k2 = infer_universe (extend x t1 ctx) t2 in
        Universe (max k1 k2)
    | Lambda ((x, t, e), _) ->
      let _ = infer_universe ctx t in
      let te = infer_type (extend x t ctx) e in
        Pi (x, t, te)
    | App (e1, e2) ->
      let (x, s, t) = infer_pi ctx e1 in
      let te = infer_type ctx e2 in
        check_equal ctx s te ;
        subst [(x, e2)] t
    | UnitType -> Universe 0
    | Unit -> UnitType

  (** [infer_universe ctx t] infers the universe level of type [t] in context [ctx]. *)
  and infer_universe ctx t =
    let u = infer_type ctx t in
      match normalize ctx u with
        | Universe k -> k
        | App _ | Var _ | Pi _ | Lambda _ | UnitType | Unit -> (err "Not a universe")

  (** [infer_pi ctx e] infers the type of [e] in context [ctx], verifies that it is
      of the form [Pi (x, t1, t2)] and returns the triple [(x, t1, t2)]. *)
  and infer_pi ctx e =
    let t = infer_type ctx e in
      match normalize ctx t with
        | Pi a -> a
        | Var _ | App _ | Universe _ | Lambda _ | UnitType | Unit -> (err "Not a pi-type") 

  (** [check_equal ctx e1 e2] checks that expressions [e1] and [e2] are equal. *)
  and check_equal ctx e1 e2 =
    if not (equal ctx e1 e2)
    then (err "Argument type does not match")

  and well_formed = function
    | [] -> ()
    | (var, t) :: ctx -> 
      let _ = infer_universe ctx t 
      in well_formed ctx


  (* TYPE CHECKING *)
  (* Infer type, check if the given type expression equals to the inferred type.*)
  let type_check ctx e t = 
    let _ = infer_universe ctx t in
    let te = infer_type ctx e in
    let t' = normalize ctx t in
      equal ctx te t'

   let translate_var = function
   	| CC.String s -> String s
   	| CC.Gensym (s, i) -> Gensym (s, i)

   let translate e =
   	let rec translate = function
	   	| CC.Var x -> Var (translate_var x)
	   	| CC.Universe i -> Universe i
	   	| CC.Pi (x, t, e) -> Pi (translate_var x, translate t, translate e)
	   	| CC.Lambda (x, t, e) -> Lambda ((translate_var x, translate t, translate e), refresh_label ())
	   	| CC.App (e1, e2) -> App (translate e1, translate e2)
	   	| CC.UnitType -> UnitType
	   	| CC.Unit -> Unit
	   in
	   	translate e

    (* [translate_ctx ctx] translates a CC context to a LCC context.
       Every type expression in the context is normalized before translation.
    *)
    let rec translate_ctx = function
      | [] -> []
      | (x, t) :: ctx -> (translate_var x, translate (CC.normalize ctx t)) :: translate_ctx ctx

end;;
