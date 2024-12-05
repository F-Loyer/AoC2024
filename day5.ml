let rec read_lines () =
  try
    let a = read_line () in
    a :: read_lines ()
  with
    End_of_file -> []

let lines = read_lines ()

let section1 = List.filter (fun s -> String.contains_from s 0 '|') lines
let section2 = List.filter (fun s -> String.contains_from s 0 ',') lines

let precedences0 = List.map (fun s -> String.split_on_char '|' s) section1
let precedences = List.map (fun l ->
                      match l with
                        [a;b] -> (int_of_string a, int_of_string b)
                      | _ -> failwith "!!!"
                    ) precedences0

let validate l =
  let rec aux already_seen l =
    match l with
      h :: tl ->
       let b = List.exists (fun x ->
                   List.exists (function a,b ->
                                  x=b && h=a 
                     ) precedences) already_seen in
       not b && aux (h::already_seen) tl
    | [] -> true
  in
  aux [] l

let middle l =
  let len = List.length l in
  List.nth l ((len-1)/2)

let sum =
  List.fold_left (+) 0

let () =
  section2
  |> List.map (String.split_on_char ',')
  |> List.map (List.map int_of_string)
  |> List.filter validate
  |> List.map middle
  |> sum
  |> print_int;
  print_char '\n'

let invalid a b =
  List.exists (fun (x,y) -> x=b && y=a) precedences 

let process arr =
  let result = ref false in
  let changed = ref true in
  while !changed do
    changed := false;
    for i=0 to Array.length arr-2 do
      for j=i+1 to Array.length arr -1 do
        if invalid arr.(i) arr.(j) then
          begin
            changed := true;
            result := true;
            let t = arr.(i) in
            arr.(i) <- arr.(j);
            arr.(j) <- t
          end
      done
    done         
  done;
  !result




let () =
  List.map (fun l ->
      let arr = String.split_on_char ',' l
                |> List.map int_of_string
                |> Array.of_list in
      if process arr then
        Array.to_list arr |> middle                               
      else
        0)
    section2
  |> sum
  |> print_int

