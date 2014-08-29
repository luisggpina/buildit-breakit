module G = Gallery
module V = Gallery.Visitor
module E = Gallery.Event
module T = Gallery.Time
module R = Gallery.Room
module P = Plain

module Entry =
    struct
        type t = V.t * E.t * T.t * R.t
        
        let create (visitor: V.t) (event: E.t) (time: T.t) (room: R.t) : t =
          (visitor, event, time, room)
    end

type t = G.t * Entry.t list

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

module M = Map.Make(G.Room)

let print_state log mode =
    let f visitor room lst =
        let (employees, guests, room_map) = lst in
        let (employees, guests) = 
        match visitor with
        | G.Visitor.Employee e -> (visitor :: employees, guests)
        | G.Visitor.Guest g -> (employees, visitor :: guests)
        in
        let room_lst =
            if (M.mem room room_map) then (M.find room room_map) else []
        in
        (employees, guests, (M.add room (visitor :: room_lst) room_map))
    in
    let g room visitor_lst lst =
        ((room,visitor_lst) :: lst)
    in
    let (employees, visitors, room_map) =
        (G.fold f (gallery log) ([], [], M.empty))
    in
    (P.print_state (employees, visitors, (M.fold g room_map [])) mode)

let print_rooms visitor log mode =
    let g lst entry =
      match entry with
        | v,E.Entry,_,R.Some r -> 
            if ((V.compare v visitor) == 0) then r :: lst else lst
        | _ -> lst
    in 
    P.print_rooms (List.fold_left g [] (entries log)) mode

let print_time visitor log =
  let gal = (gallery log) in
  let in_gal v r is_in = is_in || ((V.compare v visitor) == 0) in
  let is_in = G.fold in_gal gal false in
  let compute_time (total,last_leave) entry =
      let (v,ev,t,r) = entry in
      if ((V.compare v visitor) != 0) then
          (total,last_leave)
      else
        match ev,r with
        | E.Entry,R.Gallery -> (total + (last_leave - t), 0)
        | E.Departure,R.Gallery -> (total, t)
        | _ -> (total,last_leave)
  in
  let last_leave = if (is_in) then (G.current_time gal) else 0 in
  let start = (0,last_leave) in
  let (total,_) = (List.fold_left compute_time start (entries log)) in
  print_int total ; print_newline ()

module VS = Set.Make(V)
module RS = Set.Make(R)
module RM = Map.Make(R)

let print_occupied_rooms visitors log mode =
    let (gal,entries) = log in
    let size = (VS.cardinal visitors) in
    let create_map_from_gallery _ = 
        let f visitor room (map,results) =
            match room with
            | R.Some _ ->
                    if (VS.mem visitor visitors) then
                        let room_size = (if (RM.mem room map) then (RM.find room map) else 0) + 1 in
                        let results = if (room_size == size) then (RS.add room results) else results in
                        ((RM.add room room_size map),results)
                    else (map,results)
            | _ -> (map,results)
        in G.fold f gal (RM.empty,RS.empty)
    in
    let (room_map,results) = create_map_from_gallery () in
    let proc_entry (room_map,results) entry =
        let (v,e,t,r) = entry in
        if (RS.mem r results) then (room_map,results) 
        else
            match r with
            | R.Some _ ->
                    if (VS.mem v visitors) then
                        let occupants = if (RM.mem r room_map) then (RM.find r room_map) else 0 in
                        match e with
                        | E.Departure ->
                                let occupants = occupants + 1 in
                                if (occupants == size) then
                                    ((RM.remove r room_map),(RS.add r results))
                                else
                                    ((RM.add r occupants room_map),results)
                        | E.Entry -> 
                                ((RM.add r (occupants - 1) room_map),results)
                    else
                        (room_map,results)
            | _ -> (room_map,results)
    in
    let (_,results) = (List.fold_left proc_entry (room_map,results) entries) in
    let results_printer room lst =
        match room with
        | R.Some r -> r :: lst
        | _ -> raise Utils.Invalid_Argument
    in
    let results = RS.fold results_printer results [] in
    (P.print_rooms results mode)

let print_employees i1 i2 log mode =
    let (gal, entries) = log in
    let create_set_from_gallery _ = 
        let f visitor room set =
            match visitor,room with
            | V.Employee _, R.Gallery
            | V.Employee _, R.Some _ -> (VS.add visitor set)
            | _ -> set
        in G.fold f gal (VS.empty)
    in
    let proc_entry set entries =
        let rec within low set entries =
            match entries with
            | [] ->
                    set
            | (_,_,t,_) :: _ when t <= low ->
                    set
            | (V.Employee _ as e,_,_,R.Gallery) :: rest ->
                    within low (VS.add e set) rest
            | _ :: rest ->
                    within low set rest
        in
        let rec after (low,up) set entries =
            match entries with
            | (_,_,t,_) :: _ when t <= up ->
                    (within low set entries,set,entries)
            | (V.Employee _ as e,E.Departure,t,R.Gallery) :: rest ->
                    after (low,up) (VS.add e set) rest
            | (V.Employee _ as e,E.Entry,t,R.Gallery) :: rest ->
                    after (low,up) (VS.remove e set) rest
            | [] ->
                    (VS.empty,VS.empty,[])
            | _ :: rest ->
                    after (low,up) set rest
        in
        match i2 with
        | Some i2 ->
                let (l1,u1) = i1 in
                let (l2,u2) = i2 in
                let sorted = (List.rev (List.sort compare (u1 :: l1 :: u2 :: l2 :: []))) in
                (match sorted with
                | au1 :: al1 :: au2 :: al2 :: [] ->
                        let (a,set,entries) = after (al1,au1) set entries in
                        let (b,set,entries) = after (au2,al1) set entries in
                        let (c,set,entries) = after (al2,au2) set entries in
                        if l1 >= u2             then (VS.diff a c) else
                        if l2 >= u1             then (VS.diff c a) else
                        if u1 >= u2 && u2 >= l1 then (VS.diff (VS.inter a b) (VS.inter b c)) else
                        if u1 >= u2 && u2 >= l2 then (VS.diff (VS.inter (VS.inter a b) c) b) else
                        if u2 >= u1 && u1 >= l1 then (VS.diff b (VS.inter (VS.inter a b) c))
                                                else (VS.diff (VS.inter b c) (VS.inter a b))
                | _ -> raise Utils.Invalid_Argument)
        | None -> let (result,_,_) = after i1 set entries in result
    in
    let set = (proc_entry (create_set_from_gallery ()) entries) in
    P.print_names (VS.fold (fun x lst -> x :: lst) set []) mode
