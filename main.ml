
module L = Letter;;
module S = Score;;
module H = Hmm;;
module C = Commons;;
module V = Viterbi;;
module A = Align;;
open Tbc;;

let help_string = "Supported options: \n" ^
                  "--align <hmm-training-data> <seqfile> <outfile> : " ^
                  "aligns seqfile with profile trained by hmm-training-data to outfile \n" ^
                  "\nAll input should be in FASTA format" in
let error_string = "Command invalid - run with --help for more information" in

if (Array.length Sys.argv = 1) then (print_endline error_string; exit 1);
let cmd = String.lowercase_ascii (Array.get Sys.argv 1) in

let time_and_eval (str: string) f =
  let print_length = 44 in
  let diff_length = print_length-(String.length str)-1 in
  let rec filler_dots n str = if n = 0 then str else filler_dots (n-1) (str ^ ".") in
  print_string (str ^ " " ^ (filler_dots diff_length "") ^ " "); flush stdout;
  let (return_val, time) = C.time (fun () -> f()) in
  print_endline ("✅ - Finished in " ^ String.sub (string_of_float time) 0 4 ^ " seconds");
  return_val
in

match cmd with
| "--align" -> 
  let input_msa = Array.get Sys.argv 2 in
  let input_seqs = Array.get Sys.argv 3 in
  let out_filename = Array.get Sys.argv 4 in
  let oc = open_out out_filename in

  let processed_input_seqs = L.read_letter_matrix (open_in input_seqs) in
  let processed_msa = time_and_eval "Parsing MSA" 
  (fun () -> L.read_letter_matrix (open_in input_msa)) in
  let processed_msa_t = time_and_eval "Transposing MSA" 
  (fun () -> C.transpose (List.map (fun (s: L.fasta_seq) -> s.residues) processed_msa)) in
  let hva = time_and_eval "Profile Hidden Markov Model Construction"
  (fun () -> H.highly_variable_areas (S.is_insert_state (S.to_score_matrix (processed_msa_t)))) in
  let hmm = time_and_eval "Training model" 
  (fun () -> H.train_hmm hva processed_msa_t training_by_counting) in
  let alignment = time_and_eval "Aligning Sequences" 
  (fun () -> A.align hmm processed_input_seqs) in

  A.save alignment oc

| "--help" -> print_endline help_string; exit 0
| _ -> print_endline "Command invalid - run with --help for more information"; exit 1



