#pragma once

#define GLEW_STATIC
#include <GL/glew.h>
#include <SDL2/SDL.h>
#include <queue>
#include <string>

struct key_binder {
};

class screen {
  SDL_Window *_window;
  SDL_GLContext _gl_context;
  std::string _title;
public:
  int window_width, window_height;
  bool running;
  screen(const std::string &n_title, int n_window_width, int n_window_height
      , int multisample_samples);
  ~screen();
  void mainloop(void (*load_cb)(screen*)
      , void (*key_event_cb)(char, bool)
      , void (*mousemotion_event_cb)(float, float, int, int)
      , void (*mousebutton_event_cb)(int, bool)
      , void (*update_cb)(double, double, screen*)
      , void (*draw_cb)(double)
      , void (*cleanup_cb)(void));
  void lock_mouse();
  void unlock_mouse();
  double get_time_in_seconds();
};

