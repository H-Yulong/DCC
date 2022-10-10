module Error :
  sig
    exception Exit of int
    type info = FI of string * int * int * int * int | UNKNOWN
    type 'a withinfo = { i : info; v : 'a; }
    val dummyinfo : info
    val createInfo : string -> int -> int -> int -> int -> info
    val merge : info -> info -> info
    val errf : (unit -> 'a) -> 'b
    val printInfo : info -> unit
    val errfAt : info -> (unit -> 'a) -> 'b
    val err : string -> 'a
    val error : info -> string -> 'a
    val warning : string -> unit
    val warningAt : info -> string -> unit
  end
module Pervasive : sig type info = Error.info val pr : string -> unit end
