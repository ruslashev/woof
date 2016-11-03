#include "screen.hh"
#include "utils.hh"

screen::screen(int n_window_width, int n_window_height)
  : window_width(n_window_width), window_height(n_window_height) {
  SDL_Init(SDL_INIT_EVERYTHING);

  SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 0);

  _window = SDL_CreateWindow("woof", SDL_WINDOWPOS_CENTERED,
      SDL_WINDOWPOS_CENTERED, window_width, window_height, SDL_WINDOW_OPENGL);

  _gl_context = SDL_GL_CreateContext(_window);

  GLenum err = glewInit();
  assertf(err == GLEW_OK, "failed to initialze glew: %s",
      glewGetErrorString(err));

  assertf(GLEW_VERSION_2_0, "your graphic card does not support OpenGL 2.0");

  running = true;
}

screen::~screen() {
  SDL_GL_DeleteContext(_gl_context);
  SDL_Quit();
}

static inline char sdlkey_to_char(const SDL_Keycode &kc) {
  switch (kc) {
    case SDLK_w: return 'w';
    case SDLK_a: return 'a';
    case SDLK_s: return 's';
    case SDLK_d: return 'd';
    default:     return -1;
  }
}

void screen::mainloop(void (*load_cb)(screen*)
    , void (*key_event_cb)(char, bool)
    , void (*mousemotion_event_cb)(float, float)
    , void (*update_cb)(double, uint32_t, screen*)
    , void (*draw_cb)(void)
    , void (*cleanup_cb)(void)) {
  load_cb(this);

  uint32_t simtime = 0;
  uint64_t totalframes = 0;
  int updatecount = 0;

  while (running) {
    uint32_t real_time = SDL_GetTicks();

    { // events
      SDL_Event sdl_event;
      while (SDL_PollEvent(&sdl_event) != 0)
        if (sdl_event.type == SDL_QUIT)
          running = false;
        else if ((sdl_event.type == SDL_KEYDOWN || sdl_event.type == SDL_KEYUP)
            && sdl_event.key.repeat == 0) {
          const char key_info = sdlkey_to_char(sdl_event.key.keysym.sym);
          if (key_info != -1)
            key_event_cb(key_info, sdl_event.type == SDL_KEYDOWN);
        } else if (sdl_event.type == SDL_MOUSEMOTION)
          mousemotion_event_cb(sdl_event.motion.xrel, sdl_event.motion.yrel);
    }

    while (simtime < real_time) {
      simtime += 16;

      update_cb(16. / 1000., simtime, this);
    }

    draw_cb();

    SDL_GL_SwapWindow(_window);

    { // fps counter
      totalframes++;
      updatecount++;
      if (updatecount == 20) {
        updatecount = 0;
        uint32_t ticks_per_frame = SDL_GetTicks() - real_time;
        double fps = 1. / ((double)ticks_per_frame / 1000.)
          , fpsavg = (double)totalframes / ((double)SDL_GetTicks() / 1000.0);
        char title[256];
        snprintf(title, 256, "vfk | %2d ms/frame - %7.2f frames/s - %7.2f frames/s "
            "avg", ticks_per_frame, fps, fpsavg);
        SDL_SetWindowTitle(_window, title);
      }
    }
  }

  cleanup_cb();
}

void screen::lock_mouse() {
  SDL_SetRelativeMouseMode(SDL_TRUE);
}

void screen::unlock_mouse() {
  SDL_SetRelativeMouseMode(SDL_FALSE);
}

