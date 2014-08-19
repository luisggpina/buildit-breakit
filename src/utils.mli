exception Invalid_Argument
exception Invalid_State
exception Invalid_State
exception Authentication_Error

val unsanitized : 'a option ref -> 'a -> unit

val sanitize_int : int option ref -> int -> unit
val sanitize_ints : int list ref -> int -> unit

val sanitize_token : string option ref -> string -> unit
val sanitize_name  : string option ref -> string -> unit
val sanitize_names  : string list ref -> string -> unit

val take_argument : string option ref -> string -> unit
