all: haskell bin/sort32pairs bin/encode-kmers bin/build-index2

haskell:
	mkdir -p bin
	stack install --local-bin-path $$PWD/bin

bin/sort32pairs: src/sort32pairs.cpp
	g++ -O2 -o $@ $^

bin/encode-kmers: src/encode-kmers.cpp
	g++ -O2 -o $@ $^

bin/build-index2: src/build-index2.cpp
	g++ -O2 -o $@ $^
