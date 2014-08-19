module L = List

module Event =
    struct
        type t = Entry | Departure
    end

module Visitor =
    struct
        type t = Employee of string | Guest of string

        let name (x) =
            match x with
            | Employee e -> e
            | Guest g -> g

        let key (x) =
            match x with
            | Employee e -> "#" ^ e
            | Guest g -> "$" ^ g

        let compare (a) (b) =
            String.compare (key a) (key b)
    end

module Room =
    struct
        type t = None | Some of int | Gallery

        let outside () = None

        let inside (room: int option) = 
            match room with
            | Some room -> Some room
            | None -> Gallery

        let compare (a) (b) =
            match a,b with
            | None,None -> 0
            | Gallery,Gallery -> 0
            | _,None -> 1
            | None,_ -> -1
            | Gallery,_ -> 1
            | _,Gallery -> -1
            | Some a, Some b -> b - a
    end

module Time =
    struct
        type t = int
    end

module RM = Map.Make(Visitor)

type t = Time.t * Room.t RM.t

let empty () = (0, RM.empty)

let current_room (visitor) (gallery) =
    let (time, map) = gallery in
    if (RM.mem visitor map) then (RM.find visitor map) else Room.outside()

let current_time (gallery) =
    let (time, map) = gallery in time

let process_event (visitor) (event) (time) (room) (gallery) =
    let (cur_t,cur_map) = gallery in
    let process (cur_room) =
        match event,cur_room,room with
        | Event.Entry,Room.None,Room.Gallery -> (RM.add visitor room cur_map)
        | Event.Entry,Room.Gallery,Room.Some r -> (RM.add visitor room cur_map)
        | Event.Departure,Room.Some cur_r,Room.Some r ->
                if (cur_r == r) 
                then (RM.add visitor Room.Gallery cur_map) 
                else raise Utils.Invalid_State
        | Event.Departure,Room.Gallery,Room.Gallery -> (RM.remove visitor cur_map)
        | _ -> raise Utils.Invalid_State in
    if (time <= cur_t)
    then raise Utils.Invalid_State
    else (time, (process (current_room visitor gallery)))

let fold (f) (gallery) =
    let (time,map) = gallery in RM.fold f map
    
let from_serial (serial) =
  let (time,lst) = serial in
  let f (map)  (visitor,room) = (RM.add visitor room map) in
  (time, (L.fold_left f RM.empty (L.rev lst)))

let to_serial (gallery) =
    let f visitor room lst =
        ((visitor,room) :: lst)
    in
    let (time,map) = gallery in
    (time, (RM.fold f map []))

