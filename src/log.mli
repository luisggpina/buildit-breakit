module Entry :
    sig
        type t = Gallery.Visitor.t * Gallery.Event.t * Gallery.Time.t * Gallery.Room.t
        
        val create : Gallery.Visitor.t -> Gallery.Event.t -> Gallery.Time.t -> Gallery.Room.t -> t
    end

type t = Gallery.t * Entry.t list

exception Invalid_File

val empty : unit -> t

val process_event : Gallery.Visitor.t -> Gallery.Event.t -> Gallery.Time.t -> Gallery.Room.t -> t -> t

val gallery : t -> Gallery.t

val entries : t -> Entry.t list

val from_serial : ((Gallery.Time.t * ((Gallery.Visitor.t * Gallery.Room.t) list)) * Entry.t list) -> t

val to_serial : t -> ((Gallery.Time.t * ((Gallery.Visitor.t * Gallery.Room.t) list)) * Entry.t list)
