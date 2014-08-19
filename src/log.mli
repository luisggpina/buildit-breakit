module Entry :
    sig
        type t = Gallery.Visitor.t * Gallery.Event.t * Gallery.Time.t * Gallery.Room.t
        
        val create : Gallery.Visitor.t -> Gallery.Event.t -> Gallery.Time.t -> Gallery.Room.t -> t
    end

type t = Gallery.t * Entry.t list

val empty : unit -> t

val process_event : Gallery.Visitor.t -> Gallery.Event.t -> Gallery.Time.t -> Gallery.Room.t -> t -> t

val gallery : t -> Gallery.t

val entries : t -> Entry.t list

val from_serial : ((Gallery.Time.t * ((Gallery.Visitor.t * Gallery.Room.t) list)) * Entry.t list) -> t

val to_serial : t -> ((Gallery.Time.t * ((Gallery.Visitor.t * Gallery.Room.t) list)) * Entry.t list)

val print_state : t -> Plain.mode -> unit

val print_rooms : Gallery.Visitor.t -> t -> Plain.mode -> unit

val print_time : Gallery.Visitor.t -> t -> unit

module VS : Set.S with type elt = Gallery.Visitor.t

val print_occupied_rooms : VS.t -> t -> Plain.mode -> unit

val print_employees : (Gallery.Time.t * Gallery.Time.t) -> (Gallery.Time.t * Gallery.Time.t) option -> t -> Plain.mode -> unit
