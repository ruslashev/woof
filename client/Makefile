warnings = -Wall -Wextra -Wshadow -Wno-unused-parameter -Wno-unused-variable \
		   -Wduplicated-cond -Wdouble-promotion -Wnull-dereference \
		   -Wsuggest-attribute=const
flags = -g -std=c++0x -fno-rtti
libraries = -lSDL2 -lGLEW -lGL -lpthread

default:
	g++ main.cc screen.cc net.cc bits.cc -o woof $(flags) $(libraries) $(warnings)
	./woof

lines:
	@wc -l *.{cc,hh} Makefile ../woof_serv/src/*.{erl,hrl,src}

