exception Not_Implemented

type mode = Plain | HTML

val print_state : Gallery.Visitor.t list * Gallery.Visitor.t list * (Gallery.Room.t * Gallery.Visitor.t list) list -> mode -> unit

val print_rooms : int list -> mode -> unit

val print_names : Gallery.Visitor.t list -> mode -> unit
