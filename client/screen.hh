#pragma once

#define GLEW_STATIC
#include <GL/glew.h>
#include <SDL2/SDL.h>
#include <queue>

struct key_binder {
};

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

class event_queue {
  std::queue<event> _queue;
public:
  void push(const event &e);
  void pop(event &e);
  bool empty() const;
};

class screen {
  SDL_Window *_window;
  SDL_GLContext _gl_context;
public:
  int window_width, window_height;
  bool running;
  event_queue evq;
  screen(int n_window_width, int n_window_height);
  ~screen();
  void mainloop(void (*load_cb)(screen*)
      , void (*events_cb)(void)
      , void (*update_cb)(double, uint32_t, screen*)
      , void (*draw_cb)(void)
      , void (*cleanup_cb)(void));
  void lock_mouse();
  void unlock_mouse();
};

