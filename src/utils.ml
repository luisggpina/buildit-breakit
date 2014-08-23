
exception Invalid_Argument
exception Invalid_State
exception Authentication_Error

let unsanitized (r_x : 'a option ref) (x: 'a) : unit =
    r_x := Some x

let sanitize (f: 'a -> bool) (r_x : 'a option ref) (x: 'a) : unit =
    if (f x) then r_x := Some x else raise Invalid_Argument

let sanitize_list (f : 'a -> bool) (r_x : 'a list ref) (x : 'a) : unit =
    if (f x) then r_x := !r_x @ [ x ] else raise Invalid_Argument

let sanitize_int = sanitize (fun x -> x >= 0)

let sanitize_ints = sanitize_list (fun x -> x >= 0)

let sanitize_regexp sanitizer (r: Str.regexp) = sanitizer (fun x -> Str.string_match r x 0)

let token_regex = Str.regexp "[a-zA-Z0-9]+"
let name_regex  =  Str.regexp "[a-zA-Z]+"

let sanitize_token = sanitize_regexp sanitize token_regex
let sanitize_name  = sanitize_regexp sanitize name_regex

let sanitize_names  = sanitize_regexp sanitize_list name_regex

let take_argument (r_x : string option ref) (x : string) : unit =
    match !r_x with
    | Some _  -> raise Invalid_Argument
    | None    -> r_x := Some x
