let rec read_lines () =
  try
    let a = read_line () in
    a :: read_lines ()
  with
    End_of_file -> []

let regex = Re.Pcre.re {re|mul\((\d+),(\d+)\)|re}
          |> Re.compile

let find_all s =
  let rec aux s pos =
    try
      let g = Re.Pcre.exec ~rex:regex ~pos s in
      (Re.Group.get g 1, Re.Group.get g 2) :: aux s (Re.Group.stop g 0)
    with
      Not_found -> []
  in aux s 0

let regex = Re.Pcre.re {re|do\(\)|don't\(\)|mul\((\d+),(\d+)\)|re}
          |> Re.compile
let mult (a,b) = (int_of_string a) * (int_of_string b)

type result = Do | Dont | Mult of string * string
let find_all' s =
  let rec aux s pos =
    try
      let g = Re.Pcre.exec ~rex:regex ~pos s in
      match Re.Group.get g 0 with
        "do()" -> Do :: aux s (Re.Group.stop g 0)
      | "don't()" -> Dont :: aux s (Re.Group.stop g 0)
      | _ ->
         Mult (Re.Group.get g 1, Re.Group.get g 2) :: aux s (Re.Group.stop g 0)
    with
      Not_found -> []
  in aux s 0

let rec process l =
  match l with
    Do :: tl -> process tl
  | Dont :: tl -> process2 tl
  | Mult (a,b) :: tl -> mult (a,b) + process tl
  | [] -> 0
  and
    process2 l =
    match l with
      Do :: tl -> process tl
    | Dont :: tl -> process2 tl
    | Mult (_a,_b) :: tl -> process2 tl
    | [] -> 0

let sum =
  List.fold_left (+) 0

let lines =
  read_lines ()
let () =
  lines
  |> List.map find_all
  |> List.flatten
  |> List.map mult
  |> sum
  |> print_int
let () = print_char '\n'

let () =
  lines 
  |> List.map find_all'
  |> List.flatten
  |> process
  |> print_int
