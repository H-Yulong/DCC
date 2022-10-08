module DCC :
  sig
    exception Error of string
    type variable = String of string | Gensym of string * int
    type label = Lab of string | Labsym of string * int
    type expr =
        Var of variable
      | Universe of int
      | Label of label * expr list
      | Apply of expr * expr
      | Pi of variable * expr * expr
      | Unit
      | UnitType
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
    val mk_ctx : (label * defItem) list -> (variable * expr) list -> context
    val mk_def :
      (variable * expr) list -> variable -> expr -> expr -> expr -> defItem
    val lookup_def : label -> context -> defItem
    val lookup_var : variable -> context -> expr
    val extend_def : label -> defItem -> context -> context
    val extend_var : variable -> expr -> context -> context
    val refresh : variable -> variable
    val subst : (variable * expr) list -> expr -> expr
    val apply_prep : ('a * 'b) list -> 'c list -> ('a * 'c) list
    val normalize : context -> expr -> expr
    val subst_normal : context -> (variable * expr) list -> expr -> expr
    val equal : context -> expr -> expr -> bool
    val check_equal : context -> expr -> expr -> unit
    val check_equal_list :
      context -> (expr * expr) list -> (variable * expr) list -> unit
    val infer_type_fast : context -> expr -> expr
    val infer_universe : context -> expr -> int
    val infer_pi : context -> expr -> variable * expr * expr
    val infer_label : context -> label * expr list -> expr
    val infer_fvs : context -> (variable * expr) list -> unit
    val well_formed_def : (label * defItem) list -> unit
    val well_formed : context -> unit
    val infer_type : context -> expr -> expr
    val type_check : context -> expr -> expr -> bool
  end
