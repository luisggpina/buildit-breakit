
exception Invalid_Argument
exception Invalid_State

let unsanitized (r_x : 'a option ref) (x: 'a) : unit =
    r_x := Some x

let sanitize (f: 'a -> bool) (r_x : 'a option ref) (x: 'a) : unit =
    if (f x) then r_x := Some x else raise Invalid_Argument

let sanitize_int = sanitize (fun x -> x > 0)

let sanitize_regexp (r: Str.regexp) = sanitize (fun x -> Str.string_match r x 0)

let token_regex = Str.regexp "[a-zA-Z0-9]+"
let name_regex  =  Str.regexp "[a-zA-Z]+"

let sanitize_token = sanitize_regexp token_regex
let sanitize_name  = sanitize_regexp name_regex

let take_argument (r_x : string option ref) (x : string) : unit =
    match !r_x with
    | Some _  -> raise Invalid_Argument
    | None    -> r_x := Some x

