type probability = float

let print_matrix matrix to_string_func =
  List.iter (fun line -> print_endline (to_string_func line)) matrix

let transpose list =
  let not_empty = function | [] -> false | _  -> true in
  let non_empty_lists = List.filter not_empty list in
  let rec transpose_aux acc = function
    | [] -> acc
    | []::_ -> acc
    | m -> transpose_aux ((List.map List.hd m)::acc) (List.map List.tl m)
  in
  List.rev (transpose_aux [] non_empty_lists)

let time f =
  let t = Unix.gettimeofday () in
  let res = f () in
  (res, Unix.gettimeofday () -. t)