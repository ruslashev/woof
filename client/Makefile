warnings = -Wall -Wextra -Wshadow -Wno-unused-parameter -Wno-unused-variable \
		   -Wduplicated-cond -Wdouble-promotion -Wnull-dereference \
		   -Wsuggest-attribute=const
flags = -g -std=c++0x

default:
	g++ main.cc screen.cc net.cc bits.cc -o woof $(flags) -lSDL2 -lGLEW -lGL $(warnings)
	./woof

