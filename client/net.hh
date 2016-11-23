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

/*
enum class connection_state_type : uint8_t {
  disconnected,
  connecting,
  connection_failed,
  connected
};
*/

struct packet_header {
  uint8_t  reliable; // 1
  uint32_t sequence; // 31
  uint32_t ack_sequence;
  uint16_t client_id;
};

class connection {
  // std::queue<> reliable_messages_buffer;
  // std::queue<> unacked_reliable_messages;
  uint16_t client_id;
  net *n;
  uint32_t time_since_last_pong;
public:
  connection() {
    time_since_last_pong = 0;
    srand(time(nullptr));
    client_id = rand();
    printf("this client id is %d\n", client_id);
    n = new net(receive);
  };
  void poll(uint32_t time) {
    n->poll();
  };
  void receive(uint8_t *buffer, size_t bytes_rx) {
    print_packet(buffer, bytes_rx, "received packet");
    switch ((buffer[0] & 0b11111110) >> 1) {
      case (uint8_t)server_packet_type::ACK:
        puts("type: ACK");
        break;
      case (uint8_t)server_packet_type::PONG:
        puts("type: PONG");
        time_since_last_pong = 0;
        break;
      case (uint8_t)server_packet_type::CONNECTION_REPLY:
        puts("type: CONNECTION_REPLY");
        // net_state.client_id = ntohs(*(uint16_t*)(buffer + 1));
        // printf("assigned client id: %d\n", net_state.client_id);
        break;
      case (uint8_t)server_packet_type::ERROR:
        puts("type: ERROR");
        switch (buffer[1]) {
          case NOT_MATCHING_PROTOCOL:
            puts("type: NOT_MATCHING_PROTOCOL");
            break;
          default:
            puts("unknown type");
        }
        break;
      default:
        puts("unknown type");
    }
  }
};

void send_connection_req(net *n);

