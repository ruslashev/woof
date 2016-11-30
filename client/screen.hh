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
  inline double get_hires_time_in_seconds();
public:
  int window_width, window_height;
  bool running;
  screen(int n_window_width, int n_window_height);
  ~screen();
  void mainloop(void (*load_cb)(screen*)
      , void (*key_event_cb)(char, bool)
      , void (*mousemotion_event_cb)(float, float)
      , void (*update_cb)(double, double, screen*)
      , void (*draw_cb)(double)
      , void (*cleanup_cb)(void));
  void lock_mouse();
  void unlock_mouse();
};

