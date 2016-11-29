#include "bits.hh"
#include "utils.hh" // assertf
#include <asio.hpp> // hton*

uint8_t* bytestream::get_data() {
  return data.data();
}

size_t bytestream::get_size() {
  return data.size();
}

void bytestream::write_uint32(uint32_t value) {
  data.resize(data.size() + 4);
  *((uint32_t*)(data.data() + index)) = htonl(value);
  index += 4;
}

void bytestream::write_uint16(uint16_t value) {
  data.resize(data.size() + 2);
  *((uint16_t*)(data.data() + index)) = htons(value);
  index += 2;
}

void bytestream::write_uint8(uint8_t value) {
  data.resize(data.size() + 1);
  *(data.data() + index) = value;
  index += 1;
}

void bytestream::append(const bytestream &b) {
  uint8_t *other_data = (uint8_t*)b.data.data();
  size_t other_size = b.data.size();
  data.resize(data.size() + other_size);
  std::memcpy(data.data() + index, other_data, other_size);
  index += other_size;
}

void bytestream::clear() {
  data.clear();
  index = 0;
}

bool bytestream::empty() {
  return data.empty();
}

void bytestream::print(const char *msg) {
  print_packet(data.data(), data.size(), msg);
}

bytestream::bytestream() : index(0) {
  data.reserve(128);
}

