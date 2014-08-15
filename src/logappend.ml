module C = Core.Command
module S = Core.Command.Spec
module L = Log
module G = Gallery
module V = G.Visitor
module E = G.Event
module R = G.Room
module F = File

let batch_spec () =
    let open S in
    empty
    +> flag "-T" (required int) ~doc:"seconds Time the event is recorded"
    +> flag "-K" (required string) ~doc:"token Token used to authenticate the log entry"
    +> flag "-E" (optional string) ~doc:"string Name of employee"
    +> flag "-G" (optional string) ~doc:"string Name of guest"
    +> flag "-A" no_arg ~doc:" Arrival"
    +> flag "-L" no_arg ~doc:" Departure"
    +> flag "-R" (optional int) ~doc:"int Room ID"
    +> anon ("log" %: file)

let spec =
    let open S in
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

let parse_line t k e g a l r log =
    let visitor = match e,g with
    | Some e,None -> V.Employee e
    | None,Some g -> V.Guest g
    | _ -> raise Invalid_Argument in
    let event = match a,l with
    | true,false -> E.Entry
    | false,true -> E.Departure
    | _ -> raise Invalid_Argument in
    L.process_event visitor event t (R.inside r) log

let parse_batch b =
    let current_log : (string option * L.t) ref = ref (None,L.empty()) in
    let ic = open_in b in
    let batch_command_f t k e g a l r log_filename other =
        let log =
            (match !current_log with
            | (None,_) -> F.open_file log_filename
            | (Some log_name,log) when ((compare log_name log_filename) != 0) ->
                    F.write_file log_name log ;
                    F.open_file log_filename
            | (Some _,log) -> log
            ) in
        let log = parse_line t k e g a l r log in
        current_log := (Some log_filename,log)
    in
    try
        while true; do
            let line = input_line ic in
            let argv = Sys.argv.(0) :: (Str.split (Str.regexp " ") line) in
            let batch_command = C.basic ~summary:"" (batch_spec ()) batch_command_f in
            C.run batch_command ~argv:argv
        done;
    with 
    | End_of_file -> 
            close_in_noerr ic ;
            (match !current_log with
            | (Some log_filename,log) -> F.write_file log_filename log
            | (None,_) -> raise Invalid_Argument)
    | e -> close_in_noerr ic ; raise e

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
        match t,k,e,g,a,l,r,log_filename,b with
        | None,None,None,None,false,false,None,None,Some b -> parse_batch b
        | Some t,Some k,_,_,_,_,_,Some log_filename,None -> 
                let log = F.open_file log_filename in
                let log = parse_line t k e g a l r log in
                F.write_file log_filename log
        | _ -> raise Invalid_Argument
      )

let () =
    C.run command
