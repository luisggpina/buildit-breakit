module L = List
module V = Gallery.Visitor
module R = Gallery.Room
module S = Str

exception Not_Implemented

type mode = Plain | HTML

let rec print_list lst f sep skip_last =
  match lst with
  | [] -> ""
  | [ last ] -> (f last) ^ if not skip_last then sep else ""
  | first :: rest -> (f first) ^ sep ^ (print_list rest f sep skip_last)

let print_list_2 l1 l2 f g e0 =
  let rec aux l1 l2 =
    match l1,l2 with
    | [] , [] -> ""
    | e1 :: r1 , [] -> (f (g e1) e0) ^ (aux r1 [])
    | [] , e2 :: r2 -> (f e0 (g e2)) ^ (aux [] r2)
    | e1 :: r1 , e2 :: r2 -> (f (g e1) (g e2)) ^ (aux r1 r2) 
  in
  aux l1 l2

let print_vs vs = print_list vs V.name "," true

let print_r_v f (r,vs)=
  match r with
  | R.Some r -> (f (string_of_int r) (print_vs vs))
  | _ -> ""

let print_state_plain employees guests rooms =
  (*   Print each room row *)
  let print_r_v = print_r_v (fun r vs -> r ^ ":" ^ vs) in
  (*   Print all room lines *)
  let print_rs_vs lst = print_list lst print_r_v "\n" true in
  (*   Print both visitor lines *)
  let print_vs_vs vs_vs = print_list vs_vs print_vs "\n" true in
  (print_vs_vs (employees :: [ guests ])) ^ (print_rs_vs rooms)

let state_visitors_html_head =
"<html>
<body>
<table>
<tr>
  <th>Employee</th>
  <th>Guest</th>
</tr>
"

let print_v_html e g =
"<tr>
  <td>" ^ e ^ "</td>
  <td>" ^ g ^ "</td>
</tr>
"

let state_rooms_html_head =
"<table>
<tr>
  <th>Room ID</th>
  <th>Occupants</th>
</tr>
"

let state_rooms_html_tail =
"</table>
</body>
</html>"

let print_state_room_html r v =
"<tr>
  <td>" ^ r ^ "</td>
  <td>" ^ v ^ "</td>
</tr>
"

let print_state_html employees guests rooms =
  let print_r_v = print_r_v print_state_room_html in
  let print_rs_vs lst = print_list lst print_r_v "" false in
  state_visitors_html_head ^
  (print_list_2 employees guests print_v_html V.name "") ^
  state_rooms_html_head ^
  (print_rs_vs rooms) ^
  state_rooms_html_tail

let rec state_room_visitors visits =
    match visits with
    | [] -> ""
    | [ last ] -> (V.name last)
    | v :: rest -> (V.name v) ^ "," ^ (state_room_visitors rest)

let print_state (employees, guests, rooms) mode =
  let s =
    match mode with
    | HTML ->  print_state_html employees guests rooms
    | Plain -> print_state_plain employees guests rooms
  in
  print_string s

let print_rooms_plain rooms =
  print_list rooms string_of_int "," false

let print_rooms rooms mode =
    match mode with
    | HTML -> raise Not_Implemented
    | Plain -> print_string (print_rooms_plain rooms)

let print_names visitors mode =
    match mode with
    | HTML -> raise Not_Implemented
    | Plain -> print_string (print_vs visitors)
