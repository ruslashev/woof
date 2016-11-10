#pragma once
#include <asio.hpp>
#include "utils.hh"
#include <fstream>

static const int port_serv = 2711, port_client = 2710, max_msg_len = 256;

class net {
  asio::io_service _io;
  asio::ip::udp::socket _socket;
  asio::ip::udp::endpoint _remote_endpoint;
  char _recv_buffer[max_msg_len];
  void start_receive();
  void (*receive_cb)(char*, size_t);
public:
  net(void (*n_receive_cb)(char*, size_t));
  void send(char *message, size_t len);
  void poll();
};

enum packet_type {
  ACK = 0,
  CONNECTION_REQ = 1
};

struct packet_header {
  uint8_t reliable : 1;
  uint8_t type : 4;
  uint16_t client_id : 10;
};

void send_connection_req(net *n);

/*
 struct packet {
   uint8_t client_id : 12;
   char data[max_msg_len];
   uint16_t data_len;
   void serialize(char *buffer, size_t len);
 };
 */

