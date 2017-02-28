#include "../client/utils.hh"
#include <fstream>
#include <sstream>
#include <vector>

void load_file(const char *filename, int *success) {
  std::ifstream ifs(filename, std::ifstream::in);
  if (!filename) {
    *success = 0;
    return;
  }
  std::vector<float> vertices;
  std::vector<int> elements;
  std::string line;
  while (getline(ifs, line)) {
    if (line.substr(0, 2) == "v ") {
      std::istringstream s(line.substr(2));
      float x, y;
      s >> x;
      s >> y;
      vertices.push_back(x);
      vertices.push_back(y);
    } else if (line.substr(0, 2) == "t ") {
      std::istringstream s(line.substr(2));
      int a, b, c;
      s >> a;
      s >> b;
      s >> c;
      elements.push_back(a - 1);
      elements.push_back(b - 1);
      elements.push_back(c - 1);
    }
  }
}

