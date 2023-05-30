all: main.mlton main.mpl

main.mlton: phony
	mlton main.mlton.mlb

main.mpl: phony
	mpl main.mpl.mlb

main.mlton.dbg: phony
	mlton -const 'Exn.keepHistory true' -output main.mlton.dbg main.mlton.mlb

.PHONY: phony
phony: