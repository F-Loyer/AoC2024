

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

let () =
  let pairs = read_lines ()
          |> List.map numbers
  in
  let first = List.map fst pairs in
  let second = List.map snd pairs in
  let first' = List.sort compare first in
  let second' = List.sort compare second in
  List.combine first' second'
  |> List.map (fun (a,b) -> abs (a-b))
  |> sum
  |> print_int
