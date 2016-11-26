#pragma once

#include <cstdint> // uint*_t
#include <cstddef> // size_t
#include <vector>

struct bytestream {
  std::vector<uint8_t> data;
  size_t index;
  void write_uint8(uint8_t value);
  void write_uint16(uint16_t value);
  void write_uint32(uint32_t value);
  void print(const char *msg = "bytestream");
  bytestream();
};

