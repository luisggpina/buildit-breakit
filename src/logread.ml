module C = Core.Command
module S = Core.Command.Spec
module F = File
module L = Log
module E = Log.Entry
module G = Gallery
module V = Gallery.Visitor
module R = Gallery.Room
module EV = Gallery.Event
module P = Plain

let spec =
    let open C.Spec in
    empty
    +> flag "-K" (required string) ~doc:"token Token used to authenticate the log"
    +> flag "-H" no_arg ~doc:" Output in HTML"
    +> flag "-S" no_arg ~doc:" Print the current state"
    +> flag "-R" no_arg ~doc:" Print the list of rooms entered by an employee or guest, in chronological order"
    +> flag "-E" (listed string) ~doc:"string Name of employee"
    +> flag "-G" (listed string) ~doc:"string Name of guest"
    +> flag "-T" no_arg ~doc:" Print the time spent in a gallery by an employee or guest"
    +> flag "-I" no_arg ~doc:" Print rooms that are occupied by the specified employees/guests at the same time"
    +> flag "-A" no_arg ~doc:" List employees present during the specified time period"
    +> flag "-L" (listed int) ~doc:"int Lower time bound"
    +> flag "-U" (listed int) ~doc:"int Upper time bound"
    +> flag "-B" no_arg ~doc:" List employess present in the gallery as a whole during the first time boundary but not the second"
    +> anon ("log" %: file)

exception Invalid_Argument
exception Not_Implemented

module M = Map.Make(G.Room)

let print_state (log: L.t) : unit =
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
        (G.fold f (L.gallery log) ([], [], M.empty))
    in
    (P.print_state (employees, visitors, (M.fold g room_map [])))

let print_rooms visitor log =
    let g lst entry =
      match entry with
        | v,EV.Entry,_,R.Some r -> 
            if ((V.compare v visitor) == 0) then r :: lst else lst
        | _ -> lst
    in 
    P.print_rooms (List.fold_left g [] (L.entries log))

let print_time visitor log =
  let gal = (L.gallery log) in
  let in_gal v r is_in = is_in || ((V.compare v visitor) == 0) in
  let is_in = G.fold in_gal gal false in
  let compute_time (total,last_leave) entry =
      let (v,ev,t,r) = entry in
      if ((V.compare v visitor) != 0) then
          (total,last_leave)
      else
        match ev,r with
        | EV.Entry,R.Gallery -> (total + (last_leave - t), 0)
        | EV.Departure,R.Gallery -> (total, t)
        | _ -> (total,last_leave)
  in
  let last_leave = if (is_in) then (G.current_time gal) else 0 in
  let start = (0,last_leave) in
  let (total,_) = (List.fold_left compute_time start (L.entries log)) in
  print_int total ; print_newline ()


let command = 
    C.basic
      ~summary:"Query the data on the log file using the authentication token"
      ~readme: (fun () -> "
          A configurable tool which prints which employees are in the gallery,
          which guests are in the gallery, and the state of doors and motion
          sensors, and allows for queries of the state of the gallery. The
          log file must be authenticated with the key.")
      spec
      (fun k h s r e g t i a l u b log_filename other ->
        let log = F.open_file log_filename in
        match h,s,r,e,g,t,i,a,l,u,b with
        | true,true,false,[],[],false,false,false,[],[],false -> raise Not_Implemented
        | false,true,false,[],[],false,false,false,[],[],false -> print_state (log)
        | _,false,true,[],[],false,false,false,[],[],false -> raise Invalid_Argument
        | true,false,true,employees,guests,false,false,false,[],[],false -> raise Not_Implemented
        | false,false,true,[ employee ],[],false,false,false,[],[],false -> print_rooms (V.Employee employee) log
        | false,false,true,[],[ guest ],false,false,false,[],[],false -> print_rooms (V.Guest guest) log
        | true,false,false,employees,guests,true,false,false,[],[],false -> raise Not_Implemented
        | false,false,false,[ employee ],[],true,false,false,[],[],false -> print_time (V.Employee employee) log
        | false,false,false,[],[ guest ],true,false,false,[],[],false -> print_time (V.Guest guest) log
        | _,_,_,_,_,_,_,_,_,_,_ -> raise Invalid_Argument
      )

let () =
    C.run command
