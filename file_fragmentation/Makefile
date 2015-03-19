check: python-check rust-check

python-run:
	python python/main.py < input | tee python.out

python-check: python-run
	diff python.out output

python-clean:
	rm -f python.out

rust-run:
	rustc rust/bin.rs -o rust/bin
	rust/bin < input | tee rust.out

rust-check: rust-run
	diff rust.out output

rust-clean:
	rm -f rust.out rust/bin

clean: python-clean rust-clean

scala-run:
	mkdir -p scala/classes
	scalac -d scala/classes scala/src/main/scala/main.scala
	scala -cp scala/classes main < input > scala.out

scala-check: scala-run
	diff scala.out output

scala-clean:
	rm -rf scala/classes
	rm -f scala.out

.PHONY:
	python-run
	python-check
	rust-run
	rust-check
	scala-run
	scala-check
