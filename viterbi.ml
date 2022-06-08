open Hmm;;

module L = Letter
module C = Commons
module S = Score

exception UnexpectedEmptyHmm

type rv_tuple = 
  {match_state: C.probability; delete_state: C.probability; insert_state: C.probability}
type result_vector = rv_tuple list

let max3 a b c = max (max a b) c

let rv_tuple_to_string (rv_tuple: rv_tuple) : string = 
  "(" ^
  string_of_float (2.71828182846**rv_tuple.match_state) ^ ", " ^
  string_of_float (2.71828182846**rv_tuple.delete_state) ^ ", " ^ 
  string_of_float (2.71828182846**rv_tuple.insert_state) ^ ")"

let rv_to_string rv : string = List.fold_left (fun acc rv -> acc ^ (rv_tuple_to_string rv) ^ " ") "" rv

let apply_weights (rv_t: rv_tuple) (col: column_states) (edge_func): rv_tuple = 
  let m = rv_t.match_state +. log (Some (fst col.match_state))#?edge_func in 
  let d = rv_t.delete_state +. log col.delete_state#?edge_func in 
  let i = rv_t.insert_state +. log col.insert_state#?edge_func in
  {match_state = m; delete_state = d; insert_state = i}

let generate_rv_tuple (edge_func) (rv_t: rv_tuple) (col_states: column_states) =
  let tup = apply_weights rv_t col_states edge_func in
  max3 tup.match_state tup.delete_state tup.insert_state

let init_vector (hmm: column_states list): result_vector =
  (* m_0, d_0, i_0 *)
  let zero = {match_state=log 1.; delete_state=log 0.; insert_state=log 0.} in
  (* m_j, d_j, i_j where j = 1..K *)
  let rec init_vector_aux (hmm: column_states list) previous result =
    match hmm with
    | [] -> raise UnexpectedEmptyHmm
    | head::[] -> List.rev result
    | head::tail ->
      let d_val = generate_rv_tuple to_delete previous head in
      let next_state = {match_state=log 0.; delete_state=d_val; insert_state=log 0.} in
      init_vector_aux tail next_state (next_state::result)
  in 
  init_vector_aux hmm zero [zero]

  
let rec viterbi_procedure (hmm: column_states list) (previous_rv: result_vector) (s_i: L.letter) : result_vector =
  (* generate zero = (m_0, d_0, i_0) *)
  let begin_state = List.hd hmm in
  let rv_0 = List.hd previous_rv in
  let i_0_val = generate_rv_tuple to_insert rv_0 begin_state in
  let zero = {match_state=log 0.; delete_state=log 0.; insert_state=i_0_val} in

  let rec viterbi_aux (prev_tuple: rv_tuple) (previous_rv: result_vector) (hmm: column_states list) (result: result_vector) : result_vector =
    match hmm with
    | [] -> raise UnexpectedEmptyHmm
    | previous_column::[] -> List.rev result
    | previous_column::hmm_body -> 
      let current_column = List.hd hmm_body in
      let emission = field_or_def (snd current_column.match_state) Option.some S.zero_score in
      let ep = (S.probability_of s_i emission) /. (1. /. (float_of_int L.distinct_count)) in

      let m = log ep +. generate_rv_tuple to_match  (List.hd previous_rv)            previous_column  in
      let i =           generate_rv_tuple to_insert (List.hd (List.tl previous_rv))  current_column   in
      let d =           generate_rv_tuple to_delete prev_tuple                       previous_column  in

      let tuple = {match_state=m; delete_state=d; insert_state=i} in
      viterbi_aux tuple (List.tl previous_rv) hmm_body (tuple::result)

  in viterbi_aux zero previous_rv hmm [zero]

  
let decode (hmm: matrix) (seq: L.letters) : L.letters list =

  let result_matrix = 
    let hmm_states = List.map snd hmm in
    let init_v = init_vector hmm_states in
    let vectors = List.fold_left (fun acc s_i -> (
        match acc with
        | [] -> (viterbi_procedure hmm_states init_v s_i) :: acc
        | _ -> (viterbi_procedure hmm_states (List.hd acc) s_i) :: acc
      )) [] seq
    in
    List.fold_left (fun acc rv -> (List.rev rv) :: acc) [] (init_v::(List.rev vectors))
  in

  let mmh = List.map snd (List.rev hmm) in
  let qes = List.rev (L.GAP::seq) in

  let rec traceback mmh (rm: result_vector list) destination_func (decoded_result: L.letters list) (seq: L.letters) : L.letters list =
    match rm with
    | [] -> decoded_result
    | rv :: rest ->  
      let rv_hd = (List.hd rv) in
      let mmh_hd = (List.hd mmh) in
      let tup = apply_weights rv_hd mmh_hd destination_func in

      let new_mmh = List.tl mmh in (* Remove column from the reversed HMM *)
      let new_rest = List.map List.tl rest in (* Remove column from the rest of the result matrix *)
      let new_rv = List.tl rv in (* Remove column from the rv *)

      let traceback_to_M = lazy (traceback new_mmh new_rest to_match ([(List.hd seq)]::decoded_result) (List.tl seq)) in
      let traceback_to_D = lazy (traceback new_mmh (new_rv::new_rest) to_delete ([]::decoded_result) seq) in
      let traceback_to_I = lazy (traceback mmh rest to_insert (((List.hd seq) :: (List.hd decoded_result)) :: (List.tl decoded_result)) (List.tl seq)) in

      if tup.match_state > tup.insert_state then
        if tup.match_state > tup.delete_state
        then Lazy.force traceback_to_M
        else Lazy.force traceback_to_D
      else
        if tup.insert_state > tup.delete_state
        then Lazy.force traceback_to_I
        else Lazy.force traceback_to_D

  in
  
  traceback mmh result_matrix to_match [[]] qes

