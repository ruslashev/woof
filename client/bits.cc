#include "bits.hh"
#include "utils.hh" // assertf
#include <asio.hpp> // hton*

void bytestream::write_uint32(uint32_t value) {
  assertf(index + 4 <= size, "bytestream overflow");
  *((uint32_t*)(data + index)) = htonl(value);
  index += 4;
}

void bytestream::write_uint16(uint16_t value) {
  assertf(index + 2 <= size, "bytestream overflow");
  *((uint16_t*)(data + index)) = htons(value);
  index += 2;
}

void bytestream::write_uint8(uint8_t value) {
  assertf(index + 1 <= size, "bytestream overflow");
  *(data + index) = value;
  index += 1;
}

void bytestream::print() {
  for (size_t i = 0; i < size; i++) {
    int numbits = 8;
    while (--numbits >= 0)
      printf("%c", (data[i] & ((uint8_t)1 << numbits)) ? '1' : '0');
    printf(" ");
  }
  printf("\n");
}

