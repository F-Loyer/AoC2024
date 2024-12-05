let rec read_lines () =
  try
    let a = read_line () in
    a :: read_lines ()
  with
    End_of_file -> []

let directions =
  [|1, 0;
    1, 1;
    0, 1;
    -1, 1;
    -1, 0;
    -1, -1;
    0, -1;
    1, -1|]

let count_lines lines =
  let cols = String.length lines.(0) in
  let rows = Array.length lines in
  let s = ref 0 in
  for i = 0 to Array.length directions -1 do
    let dr,dc = directions.(i) in
    for r = 0+3*(max (-dr) 0) to rows-3*(max dr 0)-1 do
      for c = 0+3*(max (-dc) 0) to cols-3*(max dc 0)-1 do
        if lines.(r).[c] = 'X'
           && lines.(r+dr).[c+dc] = 'M'
           && lines.(r+2*dr).[c+2*dc] = 'A'
           && lines.(r+3*dr).[c+3*dc] = 'S'
        then
          incr s
      done
    done
  done;
  !s
let lines = read_lines ()
          |> Array.of_list

let count_lines' lines =
  let cols = String.length lines.(0) in
  let rows = Array.length lines in
  let s = ref 0 in
  List.iter (function (dr1, dc1, dr2, dc2) ->
               for r=1 to rows-2 do
                 for c = 1 to cols-2 do
                   if lines.(r-dr1).[c-dc1] = 'M'
                      && lines.(r).[c] = 'A'
                      && lines.(r+dr1).[c+dc1] = 'S'
                      &&lines.(r-dr2).[c-dc2] = 'M'
                      &&lines.(r+dr2).[c+dc2] = 'S'
                   then
                     incr s
                 done
               done)
    [ 1,1,  -1,1;
      1,1,  1,-1;
      -1,-1,  -1,1;
      -1,-1,  1,-1];
  !s

let () =
  count_lines lines
  |> print_int
let () = print_char '\n'

let () =
  count_lines' lines
  |> print_int
