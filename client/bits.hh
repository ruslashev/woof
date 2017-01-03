#pragma once

#include <cstdint> // uint*_t
#include <cstddef> // size_t
#include <vector>

class bytestream {
  size_t _index, _rindex;
  std::vector<uint8_t> _data;
public:
  uint8_t* data();
  size_t size() const;
  bool empty() const;
  void clear();
  void write(uint8_t value);
  void write(uint16_t value);
  void write(uint32_t value);
  void write_net(uint8_t value);
  void write_net(uint16_t value);
  void write_net(uint32_t value);
  bool read(uint8_t &value);
  bool read(uint16_t &value);
  bool read(uint32_t &value);
  bool read_net(uint8_t &value);
  bool read_net(uint16_t &value);
  bool read_net(uint32_t &value);
  void append(const bytestream &b);
  void print(const char *msg = "bytestream");
  bytestream();
  bytestream(const uint8_t *buffer, const size_t size);
};
