data.js: main.scm input
	gosh ./main.scm > $@ < input

main.scm: util.scm domino.scm
	cat $^ > $@

chicken: main.scm
	csc -O5 -o main main.scm
	time ./main > data.js < input

clean:
	rm -f data.js main.scm main
