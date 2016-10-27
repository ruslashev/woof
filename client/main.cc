#include "ogl.hh"
#include "screen.hh"
#include "utils.hh"
#include "world.hh"
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <string>

enum event_type {
  EV_EMPTY,
  EV_FORWARD,
  EV_RIGHT,
  EV_DIRECTION,
  EV_QUIT
};

struct event {
  event_type type;
  /* when type == EV_FORWARD and direction == 1, it means that entity is moving
   * forward, otherwise, if direction == -1, backward. same for EV_RIGHT. */
  int moving_direction;
  float look_direction;
};

static shaderprogram *sp;
static GLint vattr;
static array_buffer *screenverts;
static GLint resolution_unif, time_unif, modelmat_unif, color_unif;
static shader *vs, *fs;
static vertexarray *vao;

void graphics_load(screen *s) {
  s->lock_mouse();

  glClearColor(0.06f, 0.06f, 0.06f, 1);

  const char *vsrc = _glsl(
    attribute vec2 vertex_pos;
    uniform mat4 model;
    uniform mat4 projection;
    void main() {
      gl_Position = projection * model * vec4(vertex_pos, 0.0, 1.0);
    }
  );
  const char *fsrc = _glsl(
    uniform vec2 iResolution;
    uniform float iGlobalTime;
    uniform vec3 color;
    void main() {
      gl_FragColor = vec4(color, 1.0);
    }
  );

  vs = new shader(vsrc, GL_VERTEX_SHADER);
  fs = new shader(fsrc, GL_FRAGMENT_SHADER);
  sp = new shaderprogram(*vs, *fs);

  vattr = sp->bind_attrib("vertex_pos");
  resolution_unif = sp->bind_uniform("iResolution");
  modelmat_unif = sp->bind_uniform("model");
  color_unif = sp->bind_uniform("color");

  const std::vector<float> cube_verts = {
    0, 1,
    1, 0,
    0, 0,

    0, 1,
    1, 1,
    1, 0
  };
  vao = new vertexarray;
  screenverts = new array_buffer;
  vao->bind();
  screenverts->bind();
  screenverts->upload(cube_verts);
  glVertexAttribPointer(vattr, 2, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(vattr);
  vao->unbind();
  glDisableVertexAttribArray(vattr);
  screenverts->unbind();

  time_unif = sp->bind_uniform("iGlobalTime");

  sp->use_this_prog();
  glUniform2f(resolution_unif, s->window_width, s->window_height);

  glm::mat4 projection_mat = glm::ortho(0.f, (float)s->window_width
      , (float)s->window_height, 0.f, -1.f, 1.f);
  glUniformMatrix4fv(sp->bind_uniform("projection"), 1, GL_FALSE
      , glm::value_ptr(projection_mat));

  sp->dont_use_this_prog();
}

static struct player_info {
  float pos_x, pos_y;
  float rotation; // degrees
} player;
static world *w;

void load(screen *s) {
  graphics_load(s);

  player.pos_x = player.pos_y = player.rotation = 0;

  w = new world(10, 10);
}

void key_event(char key, bool down) {
  /*
  event e;
  e.type = EV_EMPTY;
  e.moving_direction = 0;
  switch (key) {
    case 'w':
      e.type = EV_FORWARD;
      e.moving_direction = 1;
      break;
    case 's':
      e.type = EV_FORWARD;
      e.moving_direction = -1;
      break;
    case 'd':
      e.type = EV_RIGHT;
      e.moving_direction = 1;
      break;
    case 'a':
      e.type = EV_RIGHT;
      e.moving_direction = -1;
      break;
    default:
      warning("invalid char");
  }
  */
  printf("%s %c\n", down? "press" : "release", key);
}

void mousemotion_event(double xrel, double yrel) {
  const float sensitivity = 2.2, m_yaw = 0.022
    , mouse_dx = xrel * sensitivity * m_yaw;
  printf("mm %f\n", mouse_dx);
}

void update(double dt, uint32_t t, screen *s) {
  sp->use_this_prog();
  glUniform1f(time_unif, (double)t / 1000.);
  sp->dont_use_this_prog();
}

void draw_square(glm::vec2 pos, glm::vec2 size, float rotation
    , glm::vec3 color) {
  glm::mat4 model;
  model = glm::translate(model, glm::vec3(pos, 0.f));
  model = glm::translate(model, glm::vec3(0.5f * size.x, 0.5f * size.y, 0.f));
  model = glm::rotate(model, rotation, glm::vec3(0.f, 0.f, 1.f));
  model = glm::translate(model, glm::vec3(-0.5f * size.x, -0.5f * size.y, 0.f));
  model = glm::scale(model, glm::vec3(size, 1.0f));

  sp->use_this_prog();
  glUniformMatrix4fv(modelmat_unif, 1, GL_FALSE, glm::value_ptr(model));
  glUniform3f(color_unif, color.x, color.y, color.z);
  vao->bind();
  glDrawArrays(GL_TRIANGLES, 0, 6);
  vao->unbind();
  sp->dont_use_this_prog();
}

void draw_world() {
  for (uint32_t y = 0; y < w->h; y++)
    for (uint32_t x = 0; x < w->w; x++) {
      uint8_t element = w->get(x, y);
      float r, g, b;
      world_palette_lookup(element, nullptr, &r, &g, &b);
      draw_square(glm::vec2(x * world_tilesize, y * world_tilesize)
          , glm::vec2(world_tilesize, world_tilesize), 0, glm::vec3(r, g, b));
    }
}

void draw() {
  glClear(GL_COLOR_BUFFER_BIT);

  draw_world();

  draw_square(glm::vec2(400 + player.pos_x, 225 + player.pos_y)
      , glm::vec2(10, 10), player.rotation, glm::vec3(1, 0, 0));
}

void cleanup() {
  delete vs;
  delete fs;
  delete sp;
  delete screenverts;
  delete vao;
  delete w;
}

int main() {
  screen s(800, 450);

  s.mainloop(load, key_event, mousemotion_event, update, draw, cleanup);

  return 0;
}

