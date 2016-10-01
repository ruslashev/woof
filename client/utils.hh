#pragma once

#include <fstream>

#define assertf(X, ...) \
  do { \
    if (!(X)) { \
      printf("assert failed in %s:%d: ", __FILE__, __LINE__); \
      printf(__VA_ARGS__); \
      puts(""); \
      exit(1); \
    } \
  } while (0)

#define die(...) do { printf(__VA_ARGS__); puts(""); exit(1); } while (0)

#define _glsl(X) "#version 120\n" #X

