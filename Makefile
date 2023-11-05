

.PHONY: all run opt dbg

all: load_nes

run:
	time ./load_nes 650 your_rom.nes > run.log

run_opt:
	time ./load_nes_opt 650 your_rom.nes > run.log

decode.cmi: decode.mli
	ocamlc -c decode.mli

decode.cmo: decode.cmi decode.ml
	ocamlc -c decode.ml

load_nes.cmo: load_nes.ml
	ocamlc -c load_nes.ml

load_nes: decode.cmi decode.cmo load_nes.cmo
	ocamlc -o load_nes decode.cmo load_nes.cmo 

opt: decode.ml decode.mli load_nes.ml
	ocamlopt -O2 -o load_nes_opt decode.ml load_nes.ml

# run with export OCAMLRUNPARAM=b
dbg: decode.ml decode.mli load_nes.ml
	ocamlopt -g -o load_nes_opt decode.ml load_nes.ml
