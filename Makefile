default:
	g++ main.cc screen.cc -o woof -g -std=c++0x -lSDL2 -lGLEW -lGL -Wall -Wextra -Wshadow -Wno-unused-parameter -Wno-unused-variable
	./woof

