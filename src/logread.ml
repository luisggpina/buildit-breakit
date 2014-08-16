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

let print_occupied_rooms employees guests log =
    let f creator set visitor = (L.VS.add (creator visitor) set) in
    let visitors = List.fold_left (f (fun x -> V.Employee x)) L.VS.empty employees in
    let visitors = List.fold_left (f (fun x -> V.Guest x)) visitors guests in
    L.print_occupied_rooms visitors log

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
        let log = F.open_file k log_filename in
        match h,s,r,e,g,t,i,a,l,u,b with
        | true,true,false,[],[],false,false,false,[],[],false -> raise Not_Implemented
        | false,true,false,[],[],false,false,false,[],[],false -> L.print_state log
        | _,false,true,[],[],false,false,false,[],[],false -> raise Invalid_Argument
        | true,false,true,employees,guests,false,false,false,[],[],false -> raise Not_Implemented
        | false,false,true,[ employee ],[],false,false,false,[],[],false -> L.print_rooms (V.Employee employee) log
        | false,false,true,[],[ guest ],false,false,false,[],[],false -> L.print_rooms (V.Guest guest) log
        | false,false,false,[ employee ],[],true,false,false,[],[],false -> L.print_time (V.Employee employee) log
        | false,false,false,[],[ guest ],true,false,false,[],[],false -> L.print_time (V.Guest guest) log
        | _,false,false,[],[],false,true,false,[],[],false -> raise Invalid_Argument
        | true,false,false,employees, guests ,false,true,false,[],[],false -> raise Not_Implemented
        | false,false,false,employees, guests ,false,true,false,[],[],false -> print_occupied_rooms employees guests log
        | true,false,false,[],[],false,false,true,[ low ],[ up ],false -> raise Not_Implemented
        | false,false,false,[],[],false,false,true,[ low ],[ up ],false 
                when up > low ->
                    L.print_employees (low,up) None log
        | true,false,false,[],[],false,false,false,low1 :: [ low2 ], up1 :: [ up2 ],true
                when up1 > low1 && up2 > low2 -> 
                    raise Not_Implemented
        | false,false,false,[],[],false,false,false,low1 :: [ low2 ], up1 :: [ up2 ],true 
                when up1 > low1 && up2 > low2 ->
                    L.print_employees (low1,up1) (Some (low2,up2)) log
        | _,_,_,_,_,_,_,_,_,_,_ -> raise Invalid_Argument
      )

let () =
    C.run command
