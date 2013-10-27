all: run-c

clean:
	rm -f a b c d *.exe *.hi *.o

run-a:
	ghc -o a a.hs -O3 -rtsopts
	./a 40000 20000 20000 10000 10 16 +RTS -sstats-a

run-b:
	ghc -o b b.hs -O3 -rtsopts
	./b 40000 20000 20000 10000 10 16 +RTS -sstats-b

run-c:
	ghc -o c c.hs -O3 -rtsopts
	./c 40000 20000 20000 10000 10 16 +RTS -sstats-c-10
	./c 40000 20000 20000 10000 11 16 +RTS -sstats-c-11
	./c 40000 20000 20000 10000 12 16 +RTS -sstats-c-12
	./c 40000 20000 20000 10000 13 16 +RTS -sstats-c-13

#run-d:
#	ghc -o d d.hs -O3 -rtsopts
#	./d 40000 20000 20000 10000 10 16 +RTS -sstats-d-10
#	./d 40000 20000 20000 10000 11 16 +RTS -sstats-d-11
#	./d 40000 20000 20000 10000 12 16 +RTS -sstats-d-12
#	./d 40000 20000 20000 10000 13 16 +RTS -sstats-d-13

