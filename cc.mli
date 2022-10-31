module CC :
  sig
    type variable = String of string | Gensym of string * int
    type expr =
        Var of variable
      | Universe of int
      | Pi of abstraction
      | Lambda of abstraction
      | App of expr * expr
      | UnitType
      | Unit
    and abstraction = variable * expr * expr
    type context = (variable * expr) list
    val lookup_ty : 'a -> ('a * 'b) list -> 'b
    val extend : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list
    val print_var : variable -> string
    val pprint : expr -> string
    val print_paren : expr -> string
    val print_env : (variable * expr) list -> string
    val refresh : variable -> variable
    val subst : (variable * expr) list -> expr -> expr
    val subst_abstraction :
      (variable * expr) list -> abstraction -> abstraction
    val normalize : (variable * expr) list -> expr -> expr
    val normalize_abstraction :
      (variable * expr) list -> abstraction -> abstraction
    val equal : (variable * expr) list -> expr -> expr -> bool
    val infer_type : (variable * expr) list -> expr -> expr
    val infer_universe : (variable * expr) list -> expr -> int
    val infer_pi : (variable * expr) list -> expr -> abstraction
    val check_equal : (variable * expr) list -> expr -> expr -> unit
    val well_formed : (variable * expr) list -> unit
    val type_check : (variable * expr) list -> expr -> expr -> bool
  end
