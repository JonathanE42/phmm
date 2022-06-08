
module H = Hmm
module L = Letter
module C = Commons
module V = Viterbi

type align_seq = { header: string; residues: string }

let split_after_n_chars (str: string) n : string list = 
  let rec split_after_n_chars_aux result str =
    if (String.length str) > n then
      let n_chars = String.sub str 0 n in
      let rest = String.sub str n ((String.length str)-n) in
      split_after_n_chars_aux (n_chars::result) rest
    else 
      List.rev (str::result)
  in split_after_n_chars_aux [] str

let align (model: H.matrix) (align_seqs: L.fasta_seq list) : align_seq list = 
  let decoded_seqs = List.fold_left (fun acc (seq: L.fasta_seq) -> acc @ [(V.decode model seq.residues)]) [] align_seqs in
  let trimmed_decoded_seqs =
    if List.for_all (fun l -> (L.to_string l) = "-") (List.map List.hd decoded_seqs) then 
      List.map List.tl decoded_seqs
    else 
      decoded_seqs
  in
    
  let max_col_lengths = List.fold_left (fun acc col -> (
    let max_length = List.fold_left (fun acc e -> if List.length e > acc then List.length e else acc) 0 col in
    acc @ [max_length]
  )) [] (C.transpose trimmed_decoded_seqs) in

  let residues : string list = 
    List.fold_left (fun acc row -> (
      acc @ 
      [(List.fold_left2 (fun acc row_col max_len -> (
          let rec add_dashes (r: L.letters) = 
            if (List.length r) = max_len then r else (add_dashes (L.GAP::r))
          in
          acc ^ (L.to_string (add_dashes row_col))
      )) "" row max_col_lengths)]
    )) [] trimmed_decoded_seqs 
  in
  
  List.fold_left2 (fun acc r h -> 
    { header=">" ^ h; residues=(String.concat "\n" (split_after_n_chars r 60)) ^ "*"}::acc
  ) 
  [] residues (List.map (fun (fasta_seq: L.fasta_seq) -> fasta_seq.header) align_seqs)
  
let save (msa: align_seq list) oc = 
  List.iter (fun str -> output_string oc (str.header ^ "\n" ^ str.residues ^ "\n")) msa;