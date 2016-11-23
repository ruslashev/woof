#pragma once
#include <asio.hpp>
#include "utils.hh"
#include <fstream>

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
  void set_endpoint(std::string hostname);
};

/*
struct connection_request { // struct for reference
  uint8_t type : 7; // CONNECTION_REQ
  uint8_t reliable : 1; // 1
  uint32_t sequence_num;
  uint8_t protocol_ver; // 1
};
struct server_packet_connection_reply { // struct for reference
  uint8_t type : 7;
  uint8_t unused : 1;
  union {
    uint16_t client_id; // CONNECTION_REPLY
    uint8_t error_type; // ERROR
  };
};
*/

enum class packet_type : uint8_t {
  ACK = 0,
  CONNECTION_REQ = 1
};

enum class server_packet_type : uint8_t {
  ACK = 0,
  PONG = 1,
  CONNECTION_REPLY = 2,
  ERROR = 3
};

enum error_type {
  NOT_MATCHING_PROTOCOL = 0
};

struct packet {
  uint8_t  reliable; // 1
  uint32_t sequence; // 31
  uint32_t ack_sequence;
  uint16_t client_id;
};

class reliable_connection {
  // std::queue<
  uint16_t client_id;
public:
  reliable_connection() {
    srand(time(nullptr));
    client_id = rand();
    printf("this client id is %d\n", client_id);
  };
};

void send_connection_req(net *n);

