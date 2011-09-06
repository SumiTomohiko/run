type t
val make : string -> string option -> t
val src_of_test : t -> string
val out_of_test : t -> string option
