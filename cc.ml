(* #use "cc.ml";; *)
(* Calculus of Constructions, with infinite universe hierarchy
   and unit type.
*)

open Err.Error

module CC = struct

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
    | Lambda of abstraction
    | App of expr * expr
    | UnitType | Unit
  and abstraction = variable * expr * expr

  type context = (variable * expr) list

  (* UTILITY FUNCTIONS *)

  (** [lookup_ty x ctx] returns the type of [x] in context [ctx]. *)
  let lookup_ty x ctx = List.assoc x ctx

  (** [extend x t ctx] returns [ctx] extended with variable [x] of type [t]. *)
  let extend x t ctx = (x, t) :: ctx

  let print_var = function
    | String s -> s
    | Gensym (s, _) -> s


  (* Pretty-printing *)
  let rec pprint = function
    | Var x -> print_var x
    | Universe i -> "U" ^ (string_of_int i)
    | Pi (x, t, e) -> Printf.sprintf "\206\160%s:%s.%s" (print_var x) (print_paren t) (pprint e)
    | Lambda (x, t, e) -> Printf.sprintf "\206\187%s:%s. %s" (print_var x) (pprint t) (pprint e)
    | App (e1, e2) -> Printf.sprintf "%s %s" (print_paren e1) (print_paren e2)
    | Unit -> "()"
    | UnitType -> "Unit"
  
  (* For wrapping parenthesis resonably *)
  and print_paren exp = match exp with
    | Lambda (x, t, e) -> Printf.sprintf "(\206\187%s:%s. %s)" (print_var x) (pprint t) (pprint e)
    | App (e1, e2) -> Printf.sprintf "(%s %s)" (print_paren e1) (print_paren e2)
    | Pi es -> "(" ^ pprint (Pi es) ^ ")"
    | _ -> pprint exp

  let rec print_env = function
    | [] -> ""
    | (x, e) :: [] -> (print_var x) ^ ":" ^ (pprint e)
    | (x, e) :: (e2 :: es) -> Printf.sprintf "%s:%s, %s" (print_var x) (pprint e) (print_env (e2 :: es))

  (* Implementation of the language *)

  (* SUBSTITUTION *)

  (** [refresh x] generates a fresh variable name whose preferred form is [x]. *)
  let refresh =
    let k = ref 0 in
      function
        | String x | Gensym (x, _) -> (incr k ; Gensym (x, !k))

  (** [subst [(x1,e1); ...; (xn;en)] e] performs the given substitution of
      expressions [e1], ..., [en] for variables [x1], ..., [xn] in expression [e]. *)
  let rec subst s = function
    | Var x -> (try List.assoc x s with Not_found -> Var x)
    | Universe k -> Universe k
    | Pi a -> Pi (subst_abstraction s a)
    | Lambda a -> Lambda (subst_abstraction s a)
    | App (e1, e2) -> App (subst s e1, subst s e2)
    | UnitType -> UnitType
    | Unit -> Unit

  and subst_abstraction s (x, t, e) =
    let x' = refresh x in
      (x', subst s t, subst ((x, Var x') :: s) e)


  (* REDUCTION *)

  (** [normalize e] normalizes the given expression [e]. It removes
      all redexes and it unfolds all definitions. It performs normalization under binders.  *)
  let rec normalize = function
    | Var x -> Var x
    | App (e1, e2) ->
      let e2 = normalize e2 in
        (match normalize e1 with
          | Lambda (x, _, e1') -> normalize (subst [(x,e2)] e1')
          | e1 -> App (e1, e2))
    | Universe k -> Universe k
    | Pi a -> Pi (normalize_abstraction a)
    | Lambda a -> Lambda(normalize_abstraction a) (* normalize_eta ctx a *)
    | UnitType -> UnitType
    | Unit -> Unit

  and normalize_abstraction (x, t, e) =
    let t = normalize t in
      (x, t, normalize e)


  (* EQUIVALENCE *)

  (** [equal e1 e2] determines whether normalized [e1] and [e2] are equivalent up to renaming
      of bound variables. *)
  let equal e1 e2 =
    let rec equal e1 e2 =
      match e1, e2 with
        | Var x1, Var x2 -> x1 = x2
        | App (e11, e12), App (e21, e22) -> equal e11 e21 && equal e12 e22
        | Universe k1, Universe k2 -> k1 = k2
        | Pi (x, t1, e1), Pi (y, t2, e2) -> equal t1 t2 && (equal e1 (subst [(y, Var x)] e2))
        | Lambda (x, t1, e1), e2 -> equal e1 (normalize (App(e2, Var x)))
        | e1, Lambda (y, t2, e2) -> equal (normalize (App(e1, Var y))) e2
        | UnitType, UnitType -> true
        | Unit, Unit -> true
        | _, _ -> false
    in
      equal (normalize e1) (normalize e2)


  (* TYPE INFERENCE *)

  (** [infer_type ctx e] infers the type of expression [e] in context [ctx].  *)
  let rec infer_type ctx = function
    | Var x -> 
      (try 
         well_formed ctx; lookup_ty x ctx
         with 
            | Not_found -> (err ("Unbound variable: " ^ (print_var x))))
    | Universe k -> well_formed ctx; Universe (k + 1)
    | Pi (x, t1, t2) ->
      let k1 = infer_universe ctx t1 in
      let k2 = infer_universe (extend x t1 ctx) t2 in
        Universe (max k1 k2)
    | Lambda (x, t, e) ->
      let _ = infer_universe ctx t in
      let te = infer_type (extend x t ctx) e in
        Pi (x, t, te)
    | App (e1, e2) ->
      let (x, s, t) = infer_pi ctx e1 in
      let te = infer_type ctx e2 in
        check_equal s te ;
        subst [(x, e2)] t
    | UnitType -> Universe 0
    | Unit -> UnitType

  (** [infer_universe ctx t] infers the universe level of type [t] in context [ctx]. *)
  and infer_universe ctx t =
    let u = infer_type ctx t in
      match normalize u with
        | Universe k -> k
        | App _ | Var _ | Pi _ | Lambda _ | UnitType | Unit -> (err "Not a universe")

  (** [infer_pi ctx e] infers the type of [e] in context [ctx], verifies that it is
      of the form [Pi (x, t1, t2)] and returns the triple [(x, t1, t2)]. *)
  and infer_pi ctx e =
    let t = infer_type ctx e in
      match normalize t with
        | Pi a -> a
        | Var _ | App _ | Universe _ | Lambda _ | UnitType | Unit -> (err "Not a pi-type")

  (** [check_equal e1 e2] checks that expressions [e1] and [e2] are equal. *)
  and check_equal e1 e2 =
    if not (equal e1 e2)
    then err "Argument type does not match"

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
    let t' = normalize t in
      equal te t'
      
end;;
