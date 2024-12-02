

let rec read_lines () =
  try
    let a = read_line () in
    a :: read_lines ()
  with
    End_of_file -> []

let numbers line =
  match line
        |> String.split_on_char ' '
        |> List.filter (fun s -> s <> "")
  with
  | [ a; b] -> (int_of_string a, int_of_string b)
  | _ -> failwith "Invalid format"

let sum =
  List.fold_left (+) 0

let pairs = read_lines ()
            |> List.map numbers
let first = List.map fst pairs
let second = List.map snd pairs

let () =
  let first' = List.sort compare first in
  let second' = List.sort compare second in
  List.combine first' second'
  |> List.map (fun (a,b) -> abs (a-b))
  |> sum
  |> print_int ;
  print_char '\n'


let count n =
  List.fold_left (fun acc m -> if n=m then acc+n else acc) 0

let () =
  List.map (fun n -> count n second) first
  |> sum
  |> print_int ;
  print_char '\n'
