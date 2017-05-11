#include "net.hh"
#include "ogl.hh"
#include "screen.hh"
#include "utils.hh"
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <string>

bool debug = false;
static shaderprogram *sp;
static GLint vattr;
static array_buffer *model_vbuf;
static GLint resolution_unif, time_unif, modelmat_unif, color_unif;
static shader *vs, *fs;
static vertexarray *vao;
static glm::vec2 model_rotation_pivot = glm::vec2(0.309016, 0.5);

static bool forward = false, backward = false, left = false, right = false
    , click = false;
static float view_angle = 0; // degrees

void graphics_load(screen *s) {
  s->lock_mouse();

  glEnable(GL_MULTISAMPLE);
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

  const std::vector<float> model_verts = {
    0,     1,
    0,     0,
    0.618, 0,

    0,     1,
    0.618, 0,
    0.618, 1,

    0.618, 1,
    0.618, 0,
    1,     0.5
  };
  vao = new vertexarray;
  model_vbuf = new array_buffer;
  vao->bind();
  model_vbuf->bind();
  model_vbuf->upload(model_verts);
  glVertexAttribPointer(vattr, 2, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(vattr);
  vao->unbind();
  glDisableVertexAttribArray(vattr);
  model_vbuf->unbind();

  time_unif = sp->bind_uniform("iGlobalTime");

  sp->use_this_prog();
  glUniform2f(resolution_unif, s->window_width, s->window_height);

  glm::mat4 projection_mat = glm::ortho(0.f, (float)s->window_width
      , (float)s->window_height, 0.f, -1.f, 1.f);
  glUniformMatrix4fv(sp->bind_uniform("projection"), 1, GL_FALSE
      , glm::value_ptr(projection_mat));
  sp->dont_use_this_prog();
}

static connection *c;

void load(screen *s) {
  graphics_load(s);

  c = new connection(port_client, s);
  c->set_endpoint("127.0.0.1", port_serv);
}

void key_event(char key, bool down) {
  switch (key) {
    case 'w':
      forward = down;
      break;
    case 's':
      backward = down;
      break;
    case 'd':
      right = down;
      break;
    case 'a':
      left = down;
      break;
    default:
      break;
  }
}

void mousemotion_event(float xrel, float yrel, int, int) {
  const float sensitivity = 2.2, m_yaw = 0.022
    , mouse_dx = xrel * sensitivity * m_yaw;
  view_angle += mouse_dx;
  if (view_angle >= 360.f)
    view_angle -= 360.f;
  if (view_angle < 0.f)
    view_angle += 360.f;
}

void mousebutton_event_cb(int button, bool down) {
  if (button == 1)
    click = down;
}

void update(double dt, double t, screen *s) {
  c->update(dt, t);

  int move, strafe;

  if (forward == backward)
    move = 0;
  else if (forward)
    move = 1;
  else if (backward)
    move = -1;

  if (right == left)
    strafe = 0;
  else if (right)
    strafe = 1;
  else if (left)
    strafe = -1;

  if (c->is_connected()) {
    movement_msg m(move, strafe, click, view_angle);
    bytestream b;
    m.serialize(b);
    c->send(b);
  }

  static bool once = false;
  if (!once) {// (!c->is_connected()) {
    once = true;
    c->connect();
  }

  sp->use_this_prog();
  glUniform1f(time_unif, t);
  sp->dont_use_this_prog();
}

void draw_model(glm::vec2 pos, glm::vec2 rotation_pivot, glm::vec2 size
    , float rotation, glm::vec3 color) {
  glm::mat4 model;
  model = glm::translate(model, glm::vec3(pos, 0.f));
  model = glm::rotate(model, rotation, glm::vec3(0.f, 0.f, 1.f));
  model = glm::scale(model, glm::vec3(size, 1.f));
  model = glm::translate(model, glm::vec3(-rotation_pivot, 0.f));

  sp->use_this_prog();
  glUniformMatrix4fv(modelmat_unif, 1, GL_FALSE, glm::value_ptr(model));
  glUniform3f(color_unif, color.x, color.y, color.z);
  vao->bind();
  glDrawArrays(GL_TRIANGLES, 0, 9);
  vao->unbind();
  sp->dont_use_this_prog();
}

void draw(double alpha) {
  glClear(GL_COLOR_BUFFER_BIT);

  for (size_t i = 0; i < c->players.size(); ++i) {
    player p = c->players[i];
    glm::vec3 color = glm::vec3(p.color_r / 255.f, p.color_g / 255.f
        , p.color_b / 255.f);
    float deserialized_view_angle = (p.view_angle / 2047.f) * (360.f - 360.f
        / 2048.f);
    draw_model(glm::vec2(p.position_x, p.position_y), model_rotation_pivot
        , glm::vec2(10, 10), deserialized_view_angle, color);
  }
}

void cleanup() {
  delete vs;
  delete fs;
  delete sp;
  delete model_vbuf;
  delete vao;
  delete c;
}

int main() {
  try {
    screen s("woof", 360, 270);

    s.mainloop(load, key_event, mousemotion_event, mousebutton_event_cb, update
        , draw, cleanup);
  } catch (const std::exception &e) {
    die("exception exit: %s", e.what());
  } catch (...) {
    die("unknown exception exit");
  }

  return 0;
}

