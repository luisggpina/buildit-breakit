module L = Log
module G = Gallery
module V = G.Visitor
module E = G.Event
module R = G.Room
module F = File
module A = Arg
module U = Utils

let t = ref None
let k = ref None
let e = ref None
let g = ref None
let a = ref false
let l = ref false
let r = ref None
let b = ref None
let log_filename = ref None

let reset_args () =
  t := None ;
  k := None ;
  e := None ;
  g := None ;
  a := false ;
  l := false ;
  r := None ;
  b := None ;
  log_filename := None

let arg_specs = A.align
[
    ("-T",  A.Int     (U.sanitize_int t),   "") ; 
    ("-K",  A.String  (U.sanitize_token k), "") ; 
    ("-E",  A.String  (U.sanitize_name e),  "") ; 
    ("-G",  A.String  (U.sanitize_name g),  "") ; 
    ("-A",  A.Set     a,                  "") ; 
    ("-L",  A.Set     l,                  "") ; 
    ("-R",  A.Int     (U.sanitize_int r),   "")
]

let batch_specs = A.align
[
    ("-B",  A.String  (U.unsanitized b),    "") ; 
]

let cur_log = ref (None,L.empty())

let log_single_use f =
    match !k,!log_filename with
    | Some k,Some log_filename ->
            let log = F.open_file k log_filename in
            F.write_file k log_filename (f log)
    | _ -> raise U.Invalid_Argument

let log_multi_use f =
    match !k,!log_filename,!cur_log with
    | Some k, Some log_filename, (None,_) ->
            cur_log := (Some (log_filename,k) , (f (F.open_file k log_filename)))
    | Some k, Some log_filename, (Some (log_name,log_k),log) ->
            if ((compare log_k k) == 0) && ((compare log_name log_filename) == 0) then
                cur_log := (Some (log_filename,k) , (f log))
            else
                begin
                    F.write_file log_k log_name log ;
                    cur_log := (Some (log_filename,k) , (f (F.open_file k log_filename)))
                end
    | _ -> raise U.Invalid_Argument

let log_multi_close () =
    (match !cur_log with
    | (Some (log_filename,log_k),log) -> F.write_file log_k log_filename log
    | (None,_) -> raise U.Invalid_Argument)

let parse_line log =
    let t = match !t with
    | Some t -> t
    | _ -> raise U.Invalid_Argument
    in
    let visitor = match !e,!g with
    | Some e,None -> V.Employee e
    | None,Some g -> V.Guest g
    | _ -> raise U.Invalid_Argument
    in
    let event = match !a,!l with
    | true,false -> E.Entry
    | false,true -> E.Departure
    | _ -> raise U.Invalid_Argument
    in
    L.process_event visitor event t (R.inside !r) log

let run_with_try f fail =
    try
      f ()
    with
    | U.Authentication_Error -> print_string "security error\n" ; fail ()
    | Arg.Bad _
    | U.Invalid_State
    | U.Invalid_Argument -> print_string "invalid\n" ; fail ()

let rec parse_batch b =
    let ic = open_in b in
    try
        while true; do
            let line = input_line ic in
            let argv = Sys.argv.(0) :: (Str.split (Str.regexp " ") line) in
            reset_args () ;
            A.parse_argv ~current:(ref 0) (Array.of_list argv) arg_specs (U.take_argument log_filename) "" ;
            run_with_try (fun () -> (log_multi_use parse_line)) (fun () -> ())
        done;
    with 
    | End_of_file -> 
            close_in_noerr ic ;
            log_multi_close ()
    | e -> close_in_noerr ic ; raise e


let main () =
    let has_args = ref false in
    let batch () =
        match !b with
        | Some b -> if not !has_args then parse_batch b else raise U.Invalid_Argument
        | _      -> raise U.Invalid_Argument
    in
    let arg () =
        reset_args () ;
        A.parse_argv Sys.argv ~current:(ref 0) arg_specs (U.take_argument log_filename) "" ;
        (log_single_use parse_line)
    in
    try
        reset_args () ;
        A.parse_argv Sys.argv ~current:(ref 0) batch_specs (fun x -> has_args := true) "" ;
        batch ()
    with
    | Arg.Bad e   -> arg ()
    | e           -> raise e

let () = run_with_try main (fun () -> exit (-1))
