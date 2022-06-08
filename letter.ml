
type letter = A | C | D | E | F | G | H | I | K | L | M | N | P | Q | R | S | T | V | W | Y | GAP
type letters = letter list
type letters_opt = letter option list
type matrix = letters list
type fasta_seq = { header: string; residues: letters}
type fasta_seqs = fasta_seq list

exception UnexpectedChar of string
exception UnexpectedEndOfLetterList

let distinct_count = 20

let to_char = function
  | A -> 'A'
  | C -> 'C'
  | D -> 'D'
  | E -> 'E'
  | F -> 'F'
  | G -> 'G'
  | H -> 'H'
  | I -> 'I'
  | K -> 'K'
  | L -> 'L'
  | M -> 'M'
  | N -> 'N'
  | P -> 'P'
  | Q -> 'Q'
  | R -> 'R'
  | S -> 'S'
  | T -> 'T'
  | V -> 'V'
  | W -> 'W'
  | Y -> 'Y'
  | GAP -> '-'

let from_char = function
  | 'A' -> A
  | 'C' -> C
  | 'D' -> D
  | 'E' -> E
  | 'F' -> F
  | 'G' -> G
  | 'H' -> H
  | 'I' -> I
  | 'K' -> K
  | 'L' -> L
  | 'M' -> M
  | 'N' -> N
  | 'P' -> P
  | 'Q' -> Q
  | 'R' -> R
  | 'S' -> S
  | 'T' -> T
  | 'V' -> V
  | 'W' -> W
  | 'Y' -> Y
  | '-' -> GAP
  | c -> raise (UnexpectedChar (String.make 1 c))

let rec skip i (input_matrix: matrix) : matrix =
  if i = 0 then input_matrix else skip (i-1) (List.tl input_matrix)

let rec get_first_n_columns n (input_matrix: matrix) : matrix =
  if n = 0 then [] else
    match input_matrix with
    | head::body -> head::(get_first_n_columns (n-1) body)
    | head -> raise UnexpectedEndOfLetterList

let rec get_first_n_letters n (letters: letters) : letters =
  if n = 0 then [] else
    match letters with
    | head::body -> head::(get_first_n_letters (n-1) body)
    | [] -> raise UnexpectedEndOfLetterList

let rec from_chars (cl: char list) : letters =
  List.rev (List.fold_left (fun acc c -> (from_char c)::acc) [] cl)

let from_string (s: string) : letters =
  let chars = List.fold_left (fun acc i -> acc @ [String.get s i]) [] (List.init (String.length s) (fun x -> x)) in
  from_chars chars

let to_string (letters: letters) : string =
  List.fold_left (fun acc letter -> acc ^ String.make 1 (to_char letter)) "" letters

let to_string_opt (letters: letters_opt) (none_char: char) : string =
  List.fold_left (fun acc (letter: letter option) -> (
    match letter with
    | Some l -> acc ^ String.make 1 (to_char l)
    | None -> acc ^ String.make 1 none_char
  )) "" letters


let read_fasta ic: (string * string) =
  let header : string = input_line ic in
  let rec read_fasta_aux result =
    try
      let first_char = input_char ic in
      match first_char with
      | '>' -> result
      | _ ->  read_fasta_aux (result ^ (String.make 1 first_char) ^ (input_line ic))
    with End_of_file -> result
  in
  (header, read_fasta_aux "")

let read_letter_matrix ic: fasta_seqs = 
  ignore (input_char ic); (* Eat first '>' in header*)

  let rec matrix_from_msa_from_channelAux (result: fasta_seqs): fasta_seqs = 
    try 
      let (header, line) = read_fasta ic in
      let remove_stars = List.filter (fun c -> c != '*') (List.init (String.length line) (String.get line)) in
      let gap_count = List.length (List.filter (fun x -> x == '-') remove_stars) in
      if gap_count > (List.length remove_stars)/2 then
        matrix_from_msa_from_channelAux result
      else (
        let letter_list = from_chars remove_stars in
        let fasta_seq = { header=header; residues=letter_list } in
        (* We prepend the letter_list and do not reverse the
           final result, as ordering does not matter (at least when fasta headers are not saved) *)
        let new_result = if List.length letter_list == 0 then result else (fasta_seq::result) in
        matrix_from_msa_from_channelAux new_result
      )
    with End_of_file -> close_in ic; result
  in matrix_from_msa_from_channelAux []


let ungapped_length (letters: letters) : int =
  List.length (List.filter (fun (l: letter) -> 
    match l with
    | GAP -> false
    | _ -> true
  ) letters)



