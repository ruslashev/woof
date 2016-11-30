warnings = -Wall -Wextra -Wshadow -Wno-unused-parameter -Wno-unused-variable \
		   -Wduplicated-cond -Wdouble-promotion -Wnull-dereference \
		   -Wsuggest-attribute=const
flags = -g -std=c++0x
libraries = -lSDL2 -lGLEW -lGL -lpthread

default:
	g++ main.cc screen.cc net.cc bits.cc -o woof $(flags) $(libraries) $(warnings)
	./woof

lines:
	@wc -l *.* Makefile ../woof_serv/src/*.{erl,src}

