module F = File
module L = Log
module E = Log.Entry
module G = Gallery
module V = Gallery.Visitor
module R = Gallery.Room
module EV = Gallery.Event
module P = Plain
module A = Arg
module U = Utils

let k = ref None
let h = ref false
let s = ref false
let r = ref false
let e = ref []
let g = ref []
let t = ref false
let i = ref false
let a = ref false
let l = ref []
let u = ref []
let b = ref false
let log_filename = ref None

let arg_specs = A.align
[
    ("-K",  A.String  (U.sanitize_token k), "") ; 
    ("-H",  A.Set     h,                    "") ; 
    ("-S",  A.Set     s,                    "") ; 
    ("-R",  A.Set     r,                    "") ; 
    ("-E",  A.String  (U.sanitize_names e), "") ; 
    ("-G",  A.String  (U.sanitize_names g), "") ; 
    ("-T",  A.Set     t,                    "") ; 
    ("-I",  A.Set     i,                    "") ; 
    ("-A",  A.Set     a,                    "") ; 
    ("-L",  A.Int     (U.sanitize_ints l),  "") ; 
    ("-U",  A.Int     (U.sanitize_ints u),  "") ; 
    ("-B",  A.Set     b,                    "")
]

exception Invalid_Argument
exception Not_Implemented

let print_occupied_rooms employees guests log =
    let f creator set visitor = (L.VS.add (creator visitor) set) in
    let visitors = List.fold_left (f (fun x -> V.Employee x)) L.VS.empty employees in
    let visitors = List.fold_left (f (fun x -> V.Guest x)) visitors guests in
    L.print_occupied_rooms visitors log
let () =
    A.parse_argv Sys.argv ~current:(ref 0) arg_specs (U.take_argument log_filename) "" ;
    let log =
        match !k,!log_filename with
        | Some k,Some log_filename -> F.open_file k log_filename
        | _                        -> raise U.Invalid_Argument
    in
    let mode = if !h then P.HTML else P.Plain in
    match mode,!s,!r,!e,!g,!t,!i,!a,!l,!u,!b with
    | _,true,false,[],[],false,false,false,[],[],false           -> L.print_state log mode
    | _,false,true,[],[],false,false,false,[],[],false           -> raise U.Invalid_Argument
    | _,false,true,[ e ],[],false,false,false,[],[],false        -> L.print_rooms (V.Employee e) log mode
    | P.Plain,false,true,[],[ g ],false,false,false,[],[],false  -> L.print_rooms (V.Guest g) log P.Plain
    | P.Plain,false,false,[ e ],[],true,false,false,[],[],false  -> L.print_time (V.Employee e) log
    | P.Plain,false,false,[],[ g ],true,false,false,[],[],false  -> L.print_time (V.Guest g) log
    | _,false,false,[],[],false,true,false,[],[],false           -> raise Invalid_Argument
    | _,false,false,es,gs,false,true,false,[],[],false           -> print_occupied_rooms es gs log mode
    | _,false,false,[],[],false,false,true,[ l ],[ u ],false
         when u > l                                              -> L.print_employees (l,u) None log mode
    | _,false,false,[],[],false,false,false,l1::[l2],u1::[u2],true
         when u1 > l1 && u2 > l2                                 -> L.print_employees (l1,u1) (Some (l2,u2)) log mode
    | _,_,_,_,_,_,_,_,_,_,_                                      -> raise Invalid_Argument
