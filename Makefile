all: rlex

rlex: rlex.rs
	rustc rlex.rs

check: automata_test
	./automata_test

automata_test:
	rustc --test automata.rs -o automata_test

clean:
	rm -f automata automata_test rlex
