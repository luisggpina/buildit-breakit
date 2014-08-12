module C = Core.Command
module S = Core.Command.Spec
module L = Log
module G = Gallery
module V = G.Visitor
module E = G.Event
module R = G.Room
module F = File

let spec =
    let open C.Spec in
    empty
    +> flag "-T" (optional int) ~doc:"seconds Time the event is recorded"
    +> flag "-K" (optional string) ~doc:"token Token used to authenticate the log entry"
    +> flag "-E" (optional string) ~doc:"string Name of employee"
    +> flag "-G" (optional string) ~doc:"string Name of guest"
    +> flag "-A" no_arg ~doc:" Arrival"
    +> flag "-L" no_arg ~doc:" Departure"
    +> flag "-R" (optional int) ~doc:"int Room ID"
    +> anon (maybe ("log" %: file))
    +> flag "-B" (optional file) ~doc:"file Batch file"

exception Invalid_Argument
exception Not_Implemented

let sanitize (f: 'a -> bool) (x: 'a) : 'a =
    if (f x) then x else raise Invalid_Argument

let sanitize_int = sanitize (fun x -> x < 0)

let sanitize_regexp (r: Str.regexp) = sanitize (fun x -> Str.string_match r x 0)

let token_regex = Str.regexp "[a-zA-Z0-9]+"
let name_regex  =  Str.regexp "[a-zA-Z]+"

let sanitize_token = sanitize_regexp token_regex
let sanitize_name  = sanitize_regexp name_regex

let command = 
    C.basic
      ~summary:"Appends data to the log file at the specified datetime using the authentication token"
      ~readme: (fun () -> "
          If the log file does not exist, logappend creates it. Otherwise,
          logappend append's to an existing log file. After logappend exits, the
          database location specified by logfile contains the information
          specified on the command line. That information can be retrieved by
          the logread tool with the appropriate options, when the token provided
          to both programs is the same.")
      spec
      (fun t k e g a l r log_filename b other ->
        let gallery = G.empty () in
          match b,t,k,e,g,a,l,r,log_filename with
          | Some b,None,None,None,None,false,false,None,None -> raise Not_Implemented
          | None,Some t,Some k,_,_,_,_,_,Some log_filename -> (
              let log = F.open_file log_filename in
              let visitor = match e,g with
              | Some e,None -> V.Employee e
              | None,Some g -> V.Guest g
              | _ -> raise Invalid_Argument in
              let event = match a,l with
              | true,false -> E.Entry
              | false,true -> E.Departure
              | _ -> raise Invalid_Argument in
              let log = (L.process_event visitor event t (R.inside r) log) in
              F.write_file log_filename log
          )
          | _ -> raise Invalid_Argument
      )

let () =
    C.run command
