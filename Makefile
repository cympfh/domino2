data.js: main.scm input
	gosh ./main.scm > $@ < input

main.scm: macros.scm util.scm domino.scm
	cat $^ > $@

main: main.scm
	csc -o $@ $^

chicken: main input
	./main > data.js < input

clean:
	rm -f data.js main.scm main
