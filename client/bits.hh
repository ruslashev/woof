#pragma once

#include <cstdint> // uint*_t
#include <cstddef> // size_t

struct bytestream {
  uint8_t *data;
  size_t size;
  size_t index;
  bool error_flag;
  void write_uint8(uint8_t value);
  void write_uint16(uint16_t value);
  void write_uint32(uint32_t value);
  void print();
  bytestream() : error_flag(0) {}
};

