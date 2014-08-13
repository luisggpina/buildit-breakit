module L = List
module V = Gallery.Visitor
module R = Gallery.Room

exception Not_Implemented

let print_state (rooms) =
    let print_name v = print_string ((V.name v) ^ ",") in
    let print_room (room,visitors) = 
        match room with
        | R.Some r -> 
                print_int r ; 
                print_string ":" ;
                (L.iter print_name visitors) ; 
                print_endline "" ;
        | _ -> ()
    in
    let (employees, guests, room_lst) = rooms in
    (L.iter print_name employees) ;
    print_endline "" ;
    (L.iter print_name guests) ;
    print_endline "" ;
    (L.iter print_room room_lst)

let print_rooms (rooms) =
  let print_room r = print_int r ; print_string ", " in 
  (L.iter print_room rooms) ;
  print_endline ""

let print_names (names) =
  let print_visitor v = 
      print_string ((match v with | V.Employee n | V.Guest n -> n) ^ ",")
  in
  (L.iter print_visitor names) ;
  print_endline ""
