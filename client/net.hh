#pragma once
#include <asio.hpp>
#include "utils.hh"
#include <fstream>

void print_packet(uint8_t *packet, size_t len, const char *msg = "packet");

static const int port_serv = 2711, port_client = 2710, max_msg_len = 256;

class net {
  asio::io_service _io;
  asio::ip::udp::socket _socket;
  asio::ip::udp::endpoint _remote_endpoint;
  uint8_t _recv_buffer[max_msg_len];
  void start_receive();
  void (*receive_cb)(uint8_t*, size_t);
public:
  net(void (*n_receive_cb)(uint8_t*, size_t));
  void send(uint8_t *message, size_t len);
  void poll();
};

enum packet_type {
  ACK = 0,
  CONNECTION_REQ = 1,
  ERROR = 2
};

enum error_type {
  NOT_MATCHING_PROTOCOL = 0
};

enum server_packet_type {
  CONNECTION_REPLY = 0
};

struct server_packet_connection_reply {
  uint8_t type : 7; // CONNECTION_REPLY
  uint8_t unused : 1;
  uint16_t client_id : 16;
};

void send_connection_req(net *n);

