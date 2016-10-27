#pragma once

class event_queue {
  std::queue<event> _queue;
public:
  void push(const event &e);
  void pop(event &e);
  bool empty() const;
};

void event_queue::push(const event &e) {
  _queue.push(e);
}

void event_queue::pop(event &e) {
  e = _queue.front();
  _queue.pop();
}

bool event_queue::empty() const {
  return _queue.empty();
}


static const inline event new_event_direction(const float &direction) { // ))
  event e;
  e.type = EV_DIRECTION;
  e.moving_direction = 0;
  e.look_direction = direction;
  return e;
}
