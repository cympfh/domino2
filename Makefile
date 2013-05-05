chicken: main input
	./main > data.js < input

data.js: main.scm input
	gosh ./main.scm > $@ < input

main.scm: macros.scm util.scm kika.scm domino.scm
	cat $^ > $@

main: main.scm
	csc -O5 -o $@ $^

clean:
	rm -f data.js main.scm main
