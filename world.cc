#include "world.hh"
#include <random>

uint64_t world::to_index(uint32_t x, uint32_t y) const {
  return y * w + x;
}

world::world(uint32_t n_w, uint32_t n_h) : w(n_w), h(n_h)
    , _data(std::vector<uint8_t>(w * h, 0)) {
  for (uint32_t y = 0; y < h; y++)
    for (uint32_t x = 0; x < w; x++) {
      _data[to_index(x, y)] = 1 + (rand() % 2);
      // printf("wrote %d\n", _data[to_index(x, y)]);
    }
}

uint8_t world::get(uint32_t x, uint32_t y) const {
  const uint64_t idx = to_index(x, y);
  return (idx < w * h) ? _data[idx] : 0;
}

void world::set(uint32_t x, uint32_t y, uint8_t element) {
  const uint64_t idx = to_index(x, y);
  if (idx < w * h)
    _data[idx] = element;
}

static const struct palette_element {
  const char *description;
  bool solid;
  struct {
    float r, g, b;
  } color;
} palette[256] = {
  /* 0 */ { "void",      0, {    0,    0,    0 } },
  /* 1 */ { "concrete",  1, { 0.63, 0.63, 0.63 } },
  /* 2 */ { "red wall",  1, { 0.86, 0.39, 0.39 } },
  /* 3 */ { "blue wall", 1, { 0.39, 0.39, 0.86 } },
  /* 4 */ { "asphalt",   0, {  0.2,  0.2,  0.2 } },
};
static const int num_defined_palette_elements = 5;

void world_palette_lookup(const uint8_t idx, bool *solid, float *color_r
    , float *color_g, float *color_b) {
  palette_element e = palette[(idx < num_defined_palette_elements) ? idx : 0];
  if (solid)
    *solid = e.solid;
  if (color_r)
    *color_r = e.color.r;
  if (color_g)
    *color_g = e.color.g;
  if (color_b)
    *color_b = e.color.b;
}

// void world_from_string(uint32_t w, uint32_t h, const std::string data) {
//   world t(w, h);
//   for
// }

