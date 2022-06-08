
buildrunclean: build

build:
	@ocamlopt unix.cmxa -o bin/main commons.ml letter.ml score.ml hmm.ml tbc.ml viterbi.ml align.ml main.ml

