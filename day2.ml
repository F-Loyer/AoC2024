

let rec read_lines () =
  try
    let a = read_line () in
    a :: read_lines ()
  with
    End_of_file -> []

let numbers line =
  String.split_on_char ' ' line
  |> List.filter (fun s -> s <> "")
  |> List.map int_of_string

(* part1 *)
let rec verif_decrease l =
  match l with
  | x :: y :: tl ->
     (y = x-1 || y = x-2 || y = x-3) && verif_decrease (y::tl)
  | _ -> true

let rec verif_increase l =
  match l with
  | x :: y :: tl ->
     (y = x+1 || y = x+2 || y = x+3) && verif_increase (y::tl)
  | _ -> true

let verif l =
  verif_decrease l || verif_increase l

let numbers_list = read_lines ()
                   |> List.map numbers

let () = numbers_list
         |> List.map verif
         |> List.filter Fun.id
         |> List.length
         |> print_int
let () = print_char '\n'

                (* part2 *)
let rec strip_line l =
  match l with
    x :: tl -> [tl] @ List.map (fun l -> x :: l) (strip_line tl)
  | _ -> []


let verif' l =
  let l_list = strip_line l in
  List.exists verif l_list

let () = numbers_list
         |> List.map verif'
         |> List.filter Fun.id
         |> List.length
         |> print_int
let () = print_char '\n'

