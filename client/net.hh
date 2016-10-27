#pragma once
#include <enet/enet.h>

const int port = 2710;

class connection {
  ENetHost *_client;
  ENetAddress _address;
  ENetPeer *_peer;
public:
  // void (*on_read_cb)(const char*, unsigned);
  // connection(void (*n_on_read_cb)(const char*, unsigned));
  connection();
  ~connection();
  void poll();
};

