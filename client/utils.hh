#pragma once

#include <fstream>
#include <cmath>
#include <cstdarg>

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
      printf("assertion `%s' failed at ", #X); \
      info() \
      printf(": "); \
      printf(__VA_ARGS__); \
      puts(""); \
      exit(1); \
    } \
  } while (0)

#define die(...) do { printf(__VA_ARGS__); puts(""); exit(1); } while (0)

#define normal_exit(...) do { printf(__VA_ARGS__); puts(""); exit(0); } while (0)

#define warning(...) do { \
  printf("warning: "); \
  printf(__VA_ARGS__); \
  puts(""); \
} while (0)

#define warning_ln(...) do { \
  printf("warning at "); \
  info() \
  printf(": "); \
  printf(__VA_ARGS__); \
  puts(""); \
} while (0)

#define _glsl(X) "#version 120\n" #X

inline float to_radians(const float &degrees) {
  return (3.14159265359f * degrees) / 180.f;
}

inline float to_degrees(const float &radians) {
  return (180.f * radians) / 3.14159265359f;
}

inline void print_packet(uint8_t *packet, size_t len
    , const char *msg = "packet") {
  printf("%s: ", msg);
  for (size_t i = 0; i < len; i++) {
    int numbits = 8;
    while (--numbits >= 0)
      printf("%c", (packet[i] & ((uint8_t)1 << numbits)) ? '1' : '0');
    printf(" ");
  }
  printf("\n");
}

inline void dputs(std::string s) {
  extern bool debug;
  if (debug)
    puts(s.c_str());
}

inline void dprintf(const char *format, ...) {
  va_list args;
  va_start(args, format);
  extern bool debug;
  if (debug)
    vprintf(format, args);
  va_end(args);
}

