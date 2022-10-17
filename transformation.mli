open Cc
open Dcc
open Labelled_cc

val transform_var : CC.variable -> DCC.variable
val transform_lab_var : LCC.variable -> DCC.variable
val remove : 'a -> 'a list -> 'a list
val remove_dup : 'a list -> 'a list
val fv_t : LCC.expr -> LCC.variable list
val fv :
  (LCC.variable * LCC.expr) list ->
  LCC.expr -> LCC.variable list
val transform_lab :
  (LCC.variable * LCC.expr) list ->
  LCC.expr -> DCC.expr
val transform_lab_ctx :
  (LCC.variable * LCC.expr) list ->
  (DCC.variable * DCC.expr) list
val merge_defs : ('a * 'b) list -> ('a * 'b) list -> ('a * 'b) list
val def_lab_full :
  (LCC.variable * LCC.expr) list ->
  LCC.expr ->
  LCC.expr -> (DCC.label * DCC.defItem) list
val def_lab :
  (LCC.variable * LCC.expr) list ->
  LCC.expr -> (DCC.label * DCC.defItem) list
val def_ctx_lab :
  (LCC.variable * LCC.expr) list ->
  (DCC.label * DCC.defItem) list
val check_type_preservation :
  (CC.variable * CC.expr) list -> CC.expr -> CC.expr -> bool
val transform_full :
  (CC.variable * CC.expr) list ->
  CC.expr -> CC.expr -> DCC.context * DCC.expr * DCC.expr
val transform :
  (CC.variable * CC.expr) list ->
  CC.expr -> DCC.context * DCC.expr * DCC.expr
