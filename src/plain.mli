exception Not_Implemented

val print_state : Gallery.Visitor.t list * Gallery.Visitor.t list * (Gallery.Room.t * Gallery.Visitor.t list) list -> unit

val print_rooms : int list -> unit

val print_names : Gallery.Visitor.t list -> unit
