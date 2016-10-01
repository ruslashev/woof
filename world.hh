#pragma once

#include <cstdint>
#include <string>
#include <vector>

class world {
  uint64_t to_index(uint32_t x, uint32_t y) const;
public:
  uint32_t w, h;
  world(uint32_t n_w, uint32_t n_h);
  uint8_t get(uint32_t x, uint32_t y) const;
  void set(uint32_t x, uint32_t y, uint8_t idx);
private:
  std::vector<uint8_t> _data; // reorder for constructor's initializer list
};

void world_palette_lookup(const uint8_t idx, bool *solid, float *color_r
    , float *color_g, float *color_b);

void world_from_string(uint32_t w, uint32_t h, const std::string data);

const int world_tilesize = 14;

