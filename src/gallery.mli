module Event :
    sig
        type t = Entry | Departure
    end

module Visitor :
    sig
        type t = Employee of string | Guest of string

        val key : t -> string

        val name : t -> string

        val compare : t -> t -> int
    end

module Room :
    sig
        type t = None | Some of int | Gallery

        val outside : unit -> t

        val inside : int option -> t

        val compare : t -> t -> int
    end

module Time :
    sig
        type t = int
    end

module RM : Map.S with type key = Visitor.t

type t

val empty : unit -> t

val current_room : Visitor.t -> t -> Room.t

val current_time : t -> Time.t

val process_event : Visitor.t -> Event.t -> Time.t -> Room.t -> t -> t

val fold : (Visitor.t -> Room.t -> 'a -> 'a) -> t -> 'a -> 'a

val to_serial : t -> (Time.t * ((Visitor.t * Room.t) list))

val from_serial : (Time.t * ((Visitor.t * Room.t) list)) -> t
