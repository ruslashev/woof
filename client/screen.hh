#pragma once

#define GLEW_STATIC
#include <GL/glew.h>
#include <SDL2/SDL.h>
#include <queue>

struct key_binder {
};

class screen {
  SDL_Window *_window;
  SDL_GLContext _gl_context;
public:
  int window_width, window_height;
  bool running;
  screen(int n_window_width, int n_window_height);
  ~screen();
  void mainloop(void (*load_cb)(screen*)
      , void (*key_event_cb)(char, bool)
      , void (*mousemotion_event_cb)(double, double)
      , void (*update_cb)(double, uint32_t, screen*)
      , void (*draw_cb)(void)
      , void (*cleanup_cb)(void));
  void lock_mouse();
  void unlock_mouse();
};
