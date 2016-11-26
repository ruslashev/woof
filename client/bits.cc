#include "bits.hh"
#include "utils.hh" // assertf
#include <asio.hpp> // hton*

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

void bytestream::print(const char *msg) {
  print_packet(data.data(), data.size(), msg);
}

bytestream::bytestream() : index(0) {
  data.reserve(128);
}

