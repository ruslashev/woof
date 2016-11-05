#pragma once
#include <asio.hpp>
#include "utils.hh"
#include <fstream>

class net {
  static const int _port_serv = 2711, _port_client = 2710, _max_msg_len = 1024;
  asio::io_service _io;
  asio::ip::udp::socket _socket;
  asio::ip::udp::endpoint _remote_endpoint;
  char _recv_buffer[_max_msg_len];
  void start_receive();
  void (*receive_cb)(char*, size_t);
public:
  net(void (*n_receive_cb)(char*, size_t));
  void send(char *message, size_t len);
  void poll();
};

