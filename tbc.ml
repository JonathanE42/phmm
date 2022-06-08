
module L = Letter
module S = Score
module C = Commons
module H = Hmm
exception EmptyColumn

type transitions = { 
  m_to_m: C.probability; m_to_d: C.probability; m_to_i: C.probability; 
  d_to_m: C.probability; d_to_d: C.probability; d_to_i: C.probability; 
  i_to_m: C.probability; i_to_d: C.probability; i_to_i: C.probability;
  }

(* Returns column_states after specified input. *)
(* skip_matem_del: Flag to whether or not if deletions and match emissions should not be counted *)
let transitions_to_states (column: L.letters) (tr: transitions) (skip_matem_and_del: bool) : H.column_states =

  let m_to_any = tr.m_to_m +. tr.m_to_d +. tr.m_to_i in
  let d_to_any = tr.d_to_m +. tr.d_to_d +. tr.d_to_i in
  let i_to_any = tr.i_to_m +. tr.i_to_d +. tr.i_to_i in

  let emission_prob = if skip_matem_and_del then None else 
    (* Distinct count is added to ungapped length due to pseudocounting *)
    (* We increment all scores (for pseudocounting reasons) except gaps *)
    let incremented_score = (S.increment_all_scores (S.from_letters column)) in
    Some (S.emission_prob S.{incremented_score with S.gap = 0.} (L.ungapped_length column + L.distinct_count))
  in
  let mat = 
    (H.{
      to_match=tr.m_to_m /. m_to_any; to_delete=Some (tr.m_to_d /. m_to_any); to_insert=Some (tr.m_to_i /. m_to_any)
    }, emission_prob) in
  let del = if skip_matem_and_del then None else
    Some H.{
      to_match=tr.d_to_m /. d_to_any; to_delete=Some (tr.d_to_d /. d_to_any); to_insert=Some (tr.d_to_i /. d_to_any)
    }
  in
  let ins =
    Some H.{
      to_match=tr.i_to_m /. i_to_any; to_delete=Some (tr.i_to_d /. i_to_any); to_insert=Some (tr.i_to_i /. i_to_any)
    }
  in

  {H.match_state=mat; H.delete_state=del; H.insert_state=ins}


let counter (previous_col) (current_col) (body) (pseudo_counts) = 
  let hva_width = List.length body in
  (* (P|CBB...B), where l.h.s of | is previous_column  *)
  let scan_area = previous_col::current_col::body in
  (* We transpose s.t. we can count in the rows*)
  let scan_area_t = C.transpose scan_area in

  let is_empty l = match l with [] -> true | _ -> false in
  let is_initializing = is_empty previous_col in
  let is_ending = is_empty current_col in

  List.fold_left (fun (t: transitions) letters -> (
    let edge_case predicate = 
      lazy (if is_ending then false else (
        if is_initializing then (List.hd letters) = L.GAP else (Lazy.force predicate)
      )) 
    in 
    let conserved_case = edge_case (lazy ((List.nth letters 1) = L.GAP)) in
    let hva_case = edge_case (lazy (List.nth letters (hva_width+1) = L.GAP)) in

    let first_letter_is_gap = ((List.hd letters) = L.GAP && (not is_initializing)) in
    let last_letter_is_gap = if hva_width = 0 then Lazy.force conserved_case else Lazy.force hva_case in
    let letters_in_hva = L.get_first_n_letters hva_width (List.tl letters) in

    let total_gaps_in_hva = (S.from_letters letters_in_hva).gap in
    let all_hva_cols_are_gap = (total_gaps_in_hva = float_of_int (List.length letters_in_hva)) in
    
    let m_to_m = Bool.to_float ((not first_letter_is_gap) && (not last_letter_is_gap) && all_hva_cols_are_gap) in
    let m_to_d = Bool.to_float ((not first_letter_is_gap) && last_letter_is_gap && all_hva_cols_are_gap) in
    let m_to_i = Bool.to_float ((not first_letter_is_gap) && (not all_hva_cols_are_gap)) in

    let i_to_m = Bool.to_float ((not all_hva_cols_are_gap) && (not last_letter_is_gap)) in
    let i_to_d = Bool.to_float ((not all_hva_cols_are_gap) && last_letter_is_gap) in
    let i_to_i = max 0. (float_of_int (List.length letters_in_hva) -. total_gaps_in_hva -. 1.) in

    let d_to_m = Bool.to_float (first_letter_is_gap && (not last_letter_is_gap) && all_hva_cols_are_gap) in
    let d_to_d = Bool.to_float (first_letter_is_gap && last_letter_is_gap && all_hva_cols_are_gap) in
    let d_to_i = Bool.to_float (first_letter_is_gap && (not all_hva_cols_are_gap)) in

    {
      m_to_m = t.m_to_m +. m_to_m; m_to_d = t.m_to_d +. m_to_d; m_to_i = t.m_to_i +. m_to_i;
      d_to_m = t.d_to_m +. d_to_m; d_to_d = t.d_to_d +. d_to_d; d_to_i = t.d_to_i +. d_to_i;
      i_to_m = t.i_to_m +. i_to_m; i_to_d = t.i_to_d +. i_to_d; i_to_i = t.i_to_i +. i_to_i;
    }
  )) pseudo_counts scan_area_t


(*
  Examines transitions from (previous_column -> current_column) and returns hmm states 
  for previous_column. That is; outgoing edges from previous_column to current_column.
*)
let training_by_counting (m: L.matrix) (hvas: (bool * int) list) : H.column_states =

  let hva_width =
    match hvas with
    | (true,n)::_ -> n (* If we start with true *)
    | (false,_)::(true,n)::_ -> n (* If we move into a HVA *)
    | (false,_)::_ -> 0 (* Otherwise we are not in a HVA *)
    | [] -> raise EmptyColumn
  in

  let pseudo_counts = 
    { 
      m_to_m = 1.; m_to_d = 1.; m_to_i = 1.;
      d_to_m = 1.; d_to_d = 1.; d_to_i = 1.;
      i_to_m = 1.; i_to_d = 1.; i_to_i = 1.;
    }
  in
  
  match m with
  | previous_column::body when (List.length hvas = 1) -> (
    (* End of MSA *)
    let current_column = [] in
    let transitions = 
      counter previous_column current_column (L.get_first_n_columns hva_width body) 
      {
        pseudo_counts with
        m_to_d = 0.; d_to_d = 0.; i_to_d = 0.;
      } in
    (* Generate with: * add delete states * add insert states * add match emissions *)
    transitions_to_states previous_column transitions false
  )
  | []::current_column::body -> (
    (* Start of MSA *)
    let previous_column = [] in
    let transitions = 
      counter previous_column current_column (L.get_first_n_columns hva_width body) 
      {
        pseudo_counts with
        d_to_m = 0.; d_to_d = 0.; d_to_i = 0.;
      } in
    (* Generate with: * no delete states * add insert states * no match emissions *)
    transitions_to_states previous_column transitions true
  )
  | previous_column::current_column::body-> (
    (* In the middle of MSA *)
    let transitions = counter previous_column current_column (L.get_first_n_columns hva_width body) pseudo_counts in
    (* Generate with: * add delete states * add insert states * add match emissions *)
    transitions_to_states previous_column transitions false
  )
  (* Our columns for training should not be empty. 
     Except the END-case, but that should be handled by the above. *)
  | [] | _::[] -> raise EmptyColumn


