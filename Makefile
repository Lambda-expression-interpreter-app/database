all: build run clean

build:
	ghc -o main Main.hs -O2

run:
	./main

.PHONY: clean
clean:
	rm -f main *.hi *.o