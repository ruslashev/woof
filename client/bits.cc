#include "bits.hh"
#include "utils.hh"
#include <asio.hpp> // hton*

uint8_t* bytestream::data() {
  return _data.data();
}

size_t bytestream::size() const {
  return _data.size();
}

bool bytestream::empty() const {
  return _data.empty();
}

void bytestream::clear() {
  _data.clear();
  _index = 0;
  _rindex = 0;
}

void bytestream::write_uint8(uint8_t value) {
  _data.resize(_data.size() + 1);
  *(_data.data() + _index) = value;
  _index += 1;
}

void bytestream::write_uint16(uint16_t value) {
  _data.resize(_data.size() + 2);
  *((uint16_t*)(_data.data() + _index)) = value;
  _index += 2;
}

void bytestream::write_uint32(uint32_t value) {
  _data.resize(_data.size() + 4);
  *((uint32_t*)(_data.data() + _index)) = value;
  _index += 4;
}

void bytestream::write_uint16_net(uint16_t value) {
  _data.resize(_data.size() + 2);
  *((uint16_t*)(_data.data() + _index)) = htons(value);
  _index += 2;
}

void bytestream::write_uint32_net(uint32_t value) {
  _data.resize(_data.size() + 4);
  *((uint32_t*)(_data.data() + _index)) = htonl(value);
  _index += 4;
}

bool bytestream::read_uint8(uint8_t &value) {
  if (_rindex + 1 <= _data.size()) {
    value = *((uint8_t*)(_data.data() + _rindex));
    _rindex += 1;
    return true;
  } else
    return false;
}

bool bytestream::read_uint16(uint16_t &value) {
  if (_rindex + 2 <= _data.size()) {
    value = *((uint16_t*)(_data.data() + _rindex));
    _rindex += 2;
    return true;
  } else
    return false;
}

bool bytestream::read_uint32(uint32_t &value) {
  if (_rindex + 4 <= _data.size()) {
    value = *((uint32_t*)(_data.data() + _rindex));
    _rindex += 4;
    return true;
  } else
    return false;
}

bool bytestream::read_uint16_net(uint16_t &value) {
  if (_rindex + 2 <= _data.size()) {
    value = ntohs(*((uint16_t*)(_data.data() + _rindex)));
    _rindex += 2;
    return true;
  } else
    return false;
}

bool bytestream::read_uint32_net(uint32_t &value) {
  if (_rindex + 4 <= _data.size()) {
    value = ntohl(*((uint32_t*)(_data.data() + _rindex)));
    _rindex += 4;
    return true;
  } else
    return false;
}

bool bytestream::read_rest(bytestream &b) {
  if (_rindex > _data.size() - 1)
    return false;
  std::vector<uint8_t> subvec(_data.begin() + _rindex, _data.end());
  b = bytestream(subvec.data(), subvec.size());
  return true;
}

void bytestream::append(const bytestream &b) {
  uint8_t *other_data = (uint8_t*)b._data.data();
  size_t other_size = b._data.size();
  _data.resize(_data.size() + other_size);
  std::memcpy(_data.data() + _index, other_data, other_size);
  _index += other_size;
}

void bytestream::print(const char *msg) {
  print_packet(_data.data(), _data.size(), msg);
}

bytestream::bytestream(const uint8_t *buffer, const size_t size)
  : _index(0)
  , _rindex(0) {
  _data.assign(buffer, buffer + size);
}

bytestream::bytestream()
  : _index(0)
  , _rindex(0) {
  _data.reserve(128);
}

