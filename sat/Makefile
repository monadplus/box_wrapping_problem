CXX  = g++ -std=c++11

all: checker

checker: checker.cc
	$(CXX) -c $< -O3
	$(CXX) -o $@ $<

clean:
	rm -f checker *.o
.PHONY: clean
