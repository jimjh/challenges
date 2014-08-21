check: python-check rust-check scala-check

python-check:
	python python/main.py < input | tee python.out
	diff python.out output

python-clean:
	rm -f python.out

rust-check:
	rustc rust/bin.rs -o rust/bin
	rust/bin < input | tee rust.out
	diff rust.out output

rust-clean:
	rm -f rust.out rust/bin

scala-run:
	mkdir -p scala/classes
	scalac -d scala/classes scala/main.scala
	scala -cp scala/classes main < input > scala.out

scala-check: scala-run
	diff scala.out output

scala-clean:
	rm -rf scala/classes
	rm -f scala.out

clean: python-clean rust-clean scala-clean

.PHONY:
	python-check
	rust-check
	scala-check
