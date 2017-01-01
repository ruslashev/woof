#pragma once

#include <cstdint> // uint*_t
#include <cstddef> // size_t
#include <vector>

class bytestream {
  size_t _index, _rindex;
  std::vector<uint8_t> _data;
public:
  uint8_t* data();
  size_t size();
  bool empty();
  void clear();
  void write_uint8(uint8_t value);
  void write_uint16(uint16_t value);
  void write_uint32(uint32_t value);
  bool read_uint8(uint8_t &value);
  bool read_uint16(uint16_t &value);
  bool read_uint32(uint32_t &value);
  void append(const bytestream &b);
  void print(const char *msg = "bytestream");
  bytestream();
  bytestream(const uint8_t *buffer, const size_t size);
};
