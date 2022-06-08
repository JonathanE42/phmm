
module L = Letter
module C = Commons

type score = 
{ 
  a: float;
  c: float;
  d: float;
  e: float;
  f: float;
  g: float;
  h: float;
  i: float;
  k: float;
  l: float;
  m: float;
  n: float;
  p: float;
  q: float;
  r: float;
  s: float;
  t: float;
  v: float;
  w: float;
  y: float;
  gap: float;
}

type scores = score list

let zero_score : score = 
  {
    a = 0.;
    c = 0.;
    d = 0.;
    e = 0.;
    f = 0.;
    g = 0.;
    h = 0.;
    i = 0.;
    k = 0.;
    l = 0.;
    m = 0.;
    n = 0.;
    p = 0.;
    q = 0.;
    r = 0.;
    s = 0.;
    t = 0.;
    v = 0.;
    w = 0.;
    y = 0.;
    gap = 0.;
  }

let rec to_string (s: score) : string = 
  "(" ^ Float.to_string s.a ^ ", "
      ^ Float.to_string s.c ^ ", "
      ^ Float.to_string s.d ^ ", "
      ^ Float.to_string s.e ^ ", "
      ^ Float.to_string s.f ^ ", "
      ^ Float.to_string s.g ^ ", "
      ^ Float.to_string s.h ^ ", "
      ^ Float.to_string s.i ^ ", "
      ^ Float.to_string s.k ^ ", "
      ^ Float.to_string s.l ^ ", "
      ^ Float.to_string s.m ^ ", "
      ^ Float.to_string s.n ^ ", "
      ^ Float.to_string s.p ^ ", "
      ^ Float.to_string s.q ^ ", "
      ^ Float.to_string s.r ^ ", "
      ^ Float.to_string s.s ^ ", "
      ^ Float.to_string s.t ^ ", "
      ^ Float.to_string s.v ^ ", "
      ^ Float.to_string s.w ^ ", "
      ^ Float.to_string s.y ^ ", "
      ^ Float.to_string s.gap ^ ")"


let from_letters (letters: L.letters) : score = 
  List.fold_left (fun score letter -> match letter with
    | L.A -> {score with a = score.a +. 1.}
    | L.C -> {score with c = score.c +. 1.}
    | L.D -> {score with d = score.d +. 1.}
    | L.E -> {score with e = score.e +. 1.}
    | L.F -> {score with f = score.f +. 1.}
    | L.G -> {score with g = score.g +. 1.}
    | L.H -> {score with h = score.h +. 1.}
    | L.I -> {score with i = score.i +. 1.}
    | L.K -> {score with k = score.k +. 1.}
    | L.L -> {score with l = score.l +. 1.}
    | L.M -> {score with m = score.m +. 1.}
    | L.N -> {score with n = score.n +. 1.}
    | L.P -> {score with p = score.p +. 1.}
    | L.Q -> {score with q = score.q +. 1.}
    | L.R -> {score with r = score.r +. 1.}
    | L.S -> {score with s = score.s +. 1.}
    | L.T -> {score with t = score.t +. 1.}
    | L.V -> {score with v = score.v +. 1.}
    | L.W -> {score with w = score.w +. 1.}
    | L.Y -> {score with y = score.y +. 1.}
    | L.GAP -> {score with gap = score.gap +. 1.}
  ) zero_score letters

let increment_all_scores (s: score) : score = 
  {
    a = s.a +. 1.;
    c = s.c +. 1.;
    d = s.d +. 1.;
    e = s.e +. 1.;
    f = s.f +. 1.;
    g = s.g +. 1.;
    h = s.h +. 1.;
    i = s.i +. 1.;
    k = s.k +. 1.;
    l = s.l +. 1.;
    m = s.m +. 1.;
    n = s.n +. 1.;
    p = s.p +. 1.;
    q = s.q +. 1.;
    r = s.r +. 1.;
    s = s.s +. 1.;
    t = s.t +. 1.;
    v = s.v +. 1.;
    w = s.w +. 1.;
    y = s.y +. 1.;
    gap = s.gap +. 1.;
  }

(* Computes score/len *)
let emission_prob (s: score) (len: int) : score = 
  let lenf = float_of_int len in
  {
    a = s.a/.lenf;
    c = s.c/.lenf;
    d = s.d/.lenf;
    e = s.e/.lenf;
    f = s.f/.lenf;
    g = s.g/.lenf;
    h = s.h/.lenf;
    i = s.i/.lenf;
    k = s.k/.lenf;
    l = s.l/.lenf;
    m = s.m/.lenf;
    n = s.n/.lenf;
    p = s.p/.lenf;
    q = s.q/.lenf;
    r = s.r/.lenf;
    s = s.s/.lenf;
    t = s.t/.lenf;
    v = s.v/.lenf;
    w = s.w/.lenf;
    y = s.y/.lenf;
    gap = s.gap/.lenf;
  }

(* Takes a letter matrix as input and outputs a score matrix; that is frequency of letters in each row *)
let to_score_matrix (letterMatrix: L.matrix) : scores  = 
  List.fold_left (fun acc letters -> acc @ [from_letters letters]) [] letterMatrix

let probability_of (l: L.letter) (s: score) : float =
  match l with
  | A -> s.a
  | C -> s.c
  | D -> s.d
  | E -> s.e
  | F -> s.f
  | G -> s.g
  | H -> s.h
  | I -> s.i
  | K -> s.k
  | L -> s.l
  | M -> s.m
  | N -> s.n
  | P -> s.p
  | Q -> s.q
  | R -> s.r
  | S -> s.s
  | T -> s.t
  | V -> s.v
  | W -> s.w
  | Y -> s.y
  | L.GAP -> s.gap


(*
  Stokastisk variabel X
  X antages at være uniform fordelt variabel, hvis vi er i insertion

  Vi laver en chi^2 test, for at tjekke om hvorvidt tallene de følger en uniform fordeling, f.eks.
  A,    C,    G,    T,    GAP
  17/5  17/5  17/5  17/5  17/5
  #linjerPåMSA/|Σ|

  med konfidens på 95%

  H_0 : Der er en sammenhæng mellem fordelingen af observationer og en ligefordeling som ovenstående beskrevet
  
*)

(* Determines whether or not a column should be an insert state *)
let is_insert_state (scores: scores) : bool list =
  let total (s: score) = 
    s.a +. s.c +. s.d +. s.e +. s.f +. s.g +. s.h +. 
    s.i +. s.k +. s.l +. s.m +. s.n +. s.p +. s.q +. 
    s.r +. s.s +. s.t +. s.v +. s.w +. s.y +. s.gap
  in
  List.fold_left (fun acc s -> (
    acc @ [(s.gap/.(total s)) > 0.5]
  )) [] scores

  (*
  (* Expected value - #msaLines/|Σ| *)
  let e = 17./.5. in
  (* Critical point, chi^2, df = 4, p = 0.05 *)
  let critical_point = 9.487729 in
  let sum_term c = ((c -. e) ** 2.)/.e in
  List.fold_left (fun acc (a,b,c,d,e) -> 
    let chi2_test_statistic = sum_term a +. sum_term b +. sum_term c +. sum_term d +. sum_term e in
    acc @ [chi2_test_statistic < critical_point]
  ) [] scores*)
