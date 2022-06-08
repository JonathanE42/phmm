
module L = Letter
module S = Score
module C = Commons

type match_edge = C.probability
type delete_edge = C.probability
type insert_edge = C.probability
exception UnexpectedHvas
exception UnexpectedState

type edges = {to_match: C.probability; to_delete: C.probability option; to_insert: C.probability option}

type column_states = 
  {match_state: edges * (S.score option); delete_state: edges option; insert_state: edges option}

type column = L.letters_opt * column_states
type matrix = column list


let field_or_def state_opt g def = Option.value (Option.bind state_opt g) ~default:def
let field_or_zero state_opt g = field_or_def state_opt g 0.

let to_delete = fun x -> x.to_delete
let to_match = fun x -> Some x.to_match
let to_insert = fun x -> x.to_insert
let ( #? ) : ('b option -> ('a -> C.probability option) -> C.probability) = field_or_zero


let rec transpose (list: matrix) : matrix = 
  let list_head_opt list = (match list with | x::xs -> x | [] -> None) in
  let list_tail_opt list = (match list with | x::xs -> xs | [] -> []) in

  match list with
  | [] -> []
  | ([], _)::xss -> transpose xss
  | ((x::xs), state)::(xss: matrix) -> (
    ((x :: List.map list_head_opt (List.map fst xss)), state)
    ::
    transpose ((xs, state) :: List.map (fun (a,b) -> (list_tail_opt a, b)) xss)
  )




(* Counts group of true-half_gaps to a tuple (true, n) for n trues in consecutive order *)
let highly_variable_areas (half_gaps: bool list) : (bool * int) list =
  (* count_trues removes trues from input until first instance of a false. 
     returns (list = remaining trues, i = how many trues were seen) *)
  let rec count_trues (half_gaps: bool list) (i: int) : (bool list * int) = 
    match half_gaps with
    | head::body -> (match head with
      | true -> count_trues body (i+1)
      | false -> (head::body, i)
    )
    | [] -> ([], i)
  in

  let rec highly_variable_areas_aux (result: (bool * int) list) half_gaps : (bool * int) list = 
    match half_gaps with
    | head::body -> (match head with
      | false -> highly_variable_areas_aux (result @ [(false, 0)]) body
      | true -> (
        let (skipped_trues, count) = (count_trues body 1) in
        highly_variable_areas_aux (result @ [(head, count)]) skipped_trues
      )
    )
    | [] -> result
  in highly_variable_areas_aux [] half_gaps


let train_hmm (hva: (bool * int) list) (m: L.matrix) (parameter_est_fun: L.matrix -> (bool * int) list -> column_states) : matrix =

  let empty_row = List.init (List.length (List.hd m)) (fun _ -> None) in
  let get_letter_row matrix = List.map (fun l -> Some l) (List.hd matrix) in

  let (fst_hva, fst_hva_n) = List.hd hva in
  let start_column = parameter_est_fun ([]::m) ((List.hd hva)::[(false, 0)]) in
  let m' = L.skip fst_hva_n m in
  let hva' = if fst_hva then List.tl hva else hva in

  let rec train_hmm_aux (hva: (bool * int) list) (m: L.matrix) (result: matrix) =
    match hva with
    | (true, _) :: _ -> (
      (* To start with a true is not possible, as a (false, true) will pop (false, true).
         We also assume, that train_hmm_aux is not called with true at the first position *)
      (* The impossibility of ending with true follows by the above and the assumption that consecutive trues are impossible *)
      raise UnexpectedHvas
    )
    (* CASE: HVA -> END *)
    | (false,_)::(true, n)::[] -> (
      let column_states = parameter_est_fun m [(true, n)] in
      train_hmm_aux [] [] (result @ [(get_letter_row m, column_states)])
    )
    (* CASE: Conserved -> END *)
    | (false,_)::[] -> (
      let column_states = parameter_est_fun m [(false, 0)] in
      train_hmm_aux [] [] (result @ [(get_letter_row m, column_states)])
    )
    (* CASE: Conserved -> HVA -> Conserved *)
    | (false, _)::(true, n)::body -> (
      let column_states = parameter_est_fun m ((false, 0)::(true, n)::[]) in
      (* Naturally we progress by one by passing body, as (true, n) will not correspond to a match state *)
      (* We also append (empty_row, empty_states) at after, for visually creating a gap in the matrix to indicate HVAs *)
      
      train_hmm_aux body (L.skip (n+1) m) (result @ [(get_letter_row m, column_states)] )
    )
    (* CASE: Conserved -> Conserved *)
    | (false, _)::(false, _)::body -> (
      let column_states = parameter_est_fun m ((false, 0)::(false, 0)::[]) in
      (* ((false, 0)::body) makes sure we only get rid of one element from HVA, s.t. we only progress by one HVA *)
      train_hmm_aux ((false, 0)::body) (List.tl m) (result @ [(get_letter_row m, column_states)])
    )
    | [] -> result
  in train_hmm_aux hva' m' [(empty_row,start_column)]



(*
  ************** String helpers ************** 
*)



let string_of_edges (edges: edges option) = 
  match edges with
  | None -> ""
  | Some edges ->
    let some_or_zero (num: C.probability option) = match num with Some n -> n | None -> 0. in
    let {to_match=m; to_delete=d; to_insert=i} = edges in
    "(" ^ (string_of_float m) ^ ", " ^ (string_of_float (some_or_zero d)) ^ ", " ^ (string_of_float (some_or_zero i)) ^ ")"

let string_of_column_states (column_states: column_states) : string =
  let {match_state=(m, em); delete_state=d; insert_state=i} = column_states in
  "M" ^ (string_of_edges (Some m)) ^ "\n" ^
  "D" ^ (string_of_edges d) ^ "\n" ^
  "I" ^ (string_of_edges i) ^ "\n" ^
  match em with
  | Some prob -> "Em: " ^ (S.to_string prob) ^ "\n"
  | None -> "" 
  

let print_hmm_probabilities (hmm: matrix) =
  List.iteri (fun index (column: column) -> (
    print_endline ("Column " ^ string_of_int (index));
    let column_states_list = snd column in
    print_endline (string_of_column_states column_states_list)
  )) hmm

let print_hmm_stats (hmm: matrix) = 
  List.iter (fun (letters_opt, states) -> (
    let k = match List.hd letters_opt with
    | Some letter -> "*"
    | None -> "I"
  in print_string k
  )) hmm; print_string "\n\n"