#pragma once

#include <fstream>
#include <cmath>

#ifdef __PRETTY_FUNCTION__
#define info() \
  printf("%s:%d in %s", __FILE__, __LINE__, __PRETTY_FUNCTION__);
#else
#define info() \
  printf("%s:%d in %s", __FILE__, __LINE__, __func__);
#endif

#define assertf(X, ...) \
  do { \
    if (!(X)) { \
      printf("assert failed in "); \
      info() \
      printf(": "); \
      printf(__VA_ARGS__); \
      puts(""); \
      exit(1); \
    } \
  } while (0)

#define die(...) do { printf(__VA_ARGS__); puts(""); exit(1); } while (0)

#define warning(...) do { \
  printf("warning at "); \
  info() \
  printf(": "); \
  printf(__VA_ARGS__); \
  puts(""); \
} while (0)

#define _glsl(X) "#version 120\n" #X

inline float to_radians(const float &degrees) {
  return (M_PI * degrees) / 180.f;
}

inline float to_degrees(const float &radians) {
  return (180.f * radians) / M_PI;
}

