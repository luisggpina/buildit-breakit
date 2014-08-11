module G = Gallery
module V = Gallery.Visitor
module E = Gallery.Event
module T = Gallery.Time
module R = Gallery.Room

module Entry =
    struct
        type t = V.t * E.t * T.t * R.t
        
        let create (visitor: V.t) (event: E.t) (time: T.t) (room: R.t) : t =
          (visitor, event, time, room)
    end

type t = G.t * Entry.t list

exception Invalid_File

let empty () : t =
  (G.empty (),[])

let process_event (visitor: V.t) (event: E.t) (time: T.t) (room: R.t) (log: t) : t =
  let (gallery,lst) = log in
  let gallery = (G.process_event visitor event time room gallery) in
  (gallery,(Entry.create visitor event time room) :: lst)

let gallery (log: t) : G.t =
  let (gal,lst) = log in gal

let entries (log: t) : Entry.t list =
  let (gal,lst) = log in lst

let from_serial (serial) =
    let (gal, lst) = serial in
    ((G.from_serial gal), lst)

let to_serial (log : t) =
    let (gal,lst) = log in
    ((G.to_serial gal),lst)
