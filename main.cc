#include "screen.hh"
#include "utils.hh"
#include "ogl.hh"
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <string>

shaderprogram *sp;
GLint vattr;
array_buffer *screenverts;
GLint resolution_unif, time_unif;
shader *vs, *fs;

void load(screen *s) {
  // vertexarray vao;

  glClearColor(0.85f, 0.f, 1.f, 1);

  std::vector<float> vertices = {
    -1.0f,  1.0f,
    -1.0f, -1.0f,
     1.0f,  1.0f,
     1.0f,  1.0f,
    -1.0f, -1.0f,
     1.0f, -1.0f
  };
  screenverts = new array_buffer;
  screenverts->upload(vertices);

  const char *vsrc = _glsl(
    attribute vec2 position;
    void main() {
      gl_Position = vec4(position, 0.0, 1.0);
    }
  );
  const char *fsrc = _glsl(
    uniform vec2 iResolution;
    uniform float iGlobalTime;
    // uniform vec3 viewOrigin;
    // uniform mat4 invProjView;
    uniform sampler3D world_data;

    void main() {
      gl_FragColor = vec4(1.0, 0.8, 1.0, 1.0);
    }
  );

  vs = new shader(vsrc, GL_VERTEX_SHADER);
  fs = new shader(fsrc, GL_FRAGMENT_SHADER);
  sp = new shaderprogram(*vs, *fs);

  vattr = sp->bind_attrib("position");
  resolution_unif = sp->bind_uniform("iResolution");

  sp->use_this_prog();
  glUniform2f(resolution_unif, s->window_width, s->window_height);
  sp->dont_use_this_prog();

  time_unif = sp->bind_uniform("iGlobalTime");
}

void update(double dt, uint32_t t, screen *s) {
  SDL_Event event;
  while (SDL_PollEvent(&event) != 0) {
    if (event.type == SDL_QUIT)
      s->running = false;
    else if (event.type == SDL_KEYDOWN || event.type == SDL_KEYUP) {
      uint8_t *keystates = (uint8_t*)SDL_GetKeyboardState(nullptr);
      int fw = keystates[SDL_SCANCODE_W] - keystates[SDL_SCANCODE_S];
      int side = keystates[SDL_SCANCODE_D] - keystates[SDL_SCANCODE_A];
    }
  }
  sp->use_this_prog();
  glUniform1f(time_unif, (double)t / 1000.);

  /*
  glm::vec3 pos = glm::vec3(cos(t / 1000.0) * 3.0f, sin(t / 1000.0) * 3.0f, 2.5f)
    , target = glm::vec3(0, 0, 0);
  glm::mat4 proj = glm::perspective(70.f,
      (float)s->window_width / (float)s->window_height, 1.f, 10.f);
  glm::mat4 view = glm::lookAt(pos, target, glm::vec3(0.f, 0.f, 1.f));
  glm::mat4 invProjView = glm::inverse(proj * view);
  glUniformMatrix4fv(sp->bind_attrib("invProjView"), 1, GL_FALSE, glm::value_ptr(invProjView));
  glUniform3f(sp->bind_attrib("viewOrigin"), pos.x, pos.y, pos.z);
  */

  sp->dont_use_this_prog();
}

void draw() {
  glClear(GL_COLOR_BUFFER_BIT);

  sp->use_this_prog();
  screenverts->bind();
  glEnableVertexAttribArray(vattr);
  glVertexAttribPointer(vattr, 2, GL_FLOAT, GL_FALSE, 0, 0);
  glDrawArrays(GL_TRIANGLES, 0, 6);
  glDisableVertexAttribArray(vattr);
  sp->dont_use_this_prog();
}

void cleanup() {
  delete vs;
  delete fs;
  delete sp;
  delete screenverts;
}

int main() {
  screen s(800, 450);

  s.mainloop(load, update, draw, cleanup);

  return 0;
}

