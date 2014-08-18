module L = List
module V = Gallery.Visitor
module R = Gallery.Room
module S = Str

exception Not_Implemented

type mode = Plain | HTML

let state_visitors_html_head =
"<html>
<body>
<table>
<tr>
  <th>Employee</th>
  <th>Guest</th>
</tr>"

let state_visitors_html_v e g =
"<tr>
  <td>" ^ e ^ "</td>
  <td>" ^ g ^ "</td>
</tr>"

let rec state_visitors employees guests =
    match employees,guests with
    | e :: e_rest , g :: g_rest -> 
            (state_visitors_html_v (V.name e) (V.name g) ) ^ (state_visitors e_rest g_rest)
    | e :: e_rest , []          ->
            (state_visitors_html_v (V.name e) "") ^ (state_visitors e_rest [])
    | [] , g :: g_rest          ->
            (state_visitors_html_v "" (V.name g) ) ^ (state_visitors [] g_rest)
    | [] , []                   ->
            "\n"

let state_rooms_html_head =
"<table>
<tr>
  <th>Room ID</th>
  <th>Occupants</th>
</tr>"

let state_rooms_html_tail =
"</table>
</body>
</html>"

let state_room_v r v =
"<tr>
  <td>" ^ (string_of_int r) ^ "</td>
  <td>" ^ v ^ "</td>
</tr>"

let rec state_room_visitors visits =
    match visits with
    | [] -> ""
    | [ last ] -> (V.name last)
    | v :: rest -> (V.name v) ^ "," ^ (state_room_visitors rest)

let rec state_rooms rooms =
    match rooms with
    | (R.Some r, visits) :: rest ->
            (state_room_v r (state_room_visitors visits)) ^ "\n" ^ (state_rooms rest)
    | (_, visits) :: rest -> state_rooms rest
    | [] -> ""

let print_state head (employees, guests) mid room_lst tail =
    print_string head ;
    print_string (state_visitors employees guests) ;
    print_string mid ;
    print_string (state_rooms room_lst) ;
    print_string tail

let print_state_plain rooms =
    let print_room (room,visitors) = 
        match room with
        | R.Some r -> 
                print_int r ; 
                print_string ":" ;
                print_string (state_room_visitors visitors) ;
                print_endline "" ;
        | _ -> ()
    in
    let (employees, guests, room_lst) = rooms in
    print_string (state_room_visitors employees) ;
    print_endline "" ;
    print_string (state_room_visitors guests) ;
    print_endline "" ;
    (L.iter print_room room_lst)

let print_state (employees, guests, rooms) mode =
    match mode with
    | HTML ->  print_state "" (employees, guests) "" rooms ""
    | Plain -> print_state_plain (employees, guests, rooms)

let print_rooms_plain rooms =
  let print_room r = print_int r ; print_string ", " in 
  (L.iter print_room rooms) ;
  print_endline ""

let print_rooms rooms mode =
    match mode with
    | HTML -> raise Not_Implemented
    | Plain -> print_rooms_plain rooms

let print_names_plain names =
  let print_visitor v = 
      print_string ((match v with | V.Employee n | V.Guest n -> n) ^ ",")
  in
  (L.iter print_visitor names) ;
  print_endline ""

let print_names names mode =
    match mode with
    | HTML -> raise Not_Implemented
    | Plain -> print_names_plain names
