CXX  = g++ -std=c++11
DIR  = /usr/local
LIBS = -lgecodedriver    -lgecodesearch  \
       -lgecodeminimodel -lgecodeint     \
       -lgecodekernel    -lgecodesupport

all: p checker

p: box.cc
	$(CXX) -c $< -O3
	$(CXX) -o $@ $< $(LIBS)

checker: checker.cc
	$(CXX) -c $< -O3
	$(CXX) -o $@ $< $(LIBS)

clean:
	rm -f p checker *.o
