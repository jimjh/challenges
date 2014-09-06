check: python-check rust-check scala-check

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

scala-run:
	mkdir -p scala/classes
	scalac -classpath 'scala/lib/*' -d scala/classes scala/main.scala
	scala -cp 'target/scala-2.10/classes:scala/lib/*' main < input | tee scala.out

scala-check: scala-run
	diff scala.out output

scala-clean:
	rm -rf scala/classes
	rm -f scala.out

clean: python-clean rust-clean scala-clean

.PHONY:
	python-run
	python-check
	rust-run
	rust-check
	scala-run
	scala-check
