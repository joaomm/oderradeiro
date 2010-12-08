SCALA = scala-2.8
SCALAC = scalac-2.8

compile: ep3-joao-pedro.scala
	$(SCALAC) ep3-joao-pedro.scala

tests: compile
	$(SCALA) tests.scala

clean:
	rm -Rf pfc