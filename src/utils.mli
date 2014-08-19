exception Invalid_Argument
exception Invalid_State

val unsanitized : 'a option ref -> 'a -> unit

val sanitize_int : int option ref -> int -> unit

val sanitize_token : string option ref -> string -> unit
val sanitize_name  : string option ref -> string -> unit

val take_argument : string option ref -> string -> unit
