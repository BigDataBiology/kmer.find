all: bin/sort32pairs bin/encode-kmers bin/build-index2 bin/query-kmers

bin/sort32pairs: src/sort32pairs.cpp
	g++ -O2 -o $@ $^

bin/encode-kmers: src/encode-kmers.cpp
	g++ -O2 -o $@ $^

bin/build-index2: src/build-index2.cpp
	g++ -O2 -o $@ $^

bin/query-kmers: src/query-kmers.cpp
	g++ -O2 -o $@ $^
