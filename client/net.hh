#pragma once
#include <asio.hpp>
#include "utils.hh"
#include "bits.hh"
#include <queue>

static const int port_serv = 2711, port_client = 2710, max_msg_len = 256;

class net {
  asio::io_service _io;
  asio::ip::udp::socket _socket;
  asio::ip::udp::endpoint _remote_endpoint;
  uint8_t _recv_buffer[max_msg_len];
  void start_receive();
  void (*receive_cb)(void*, uint8_t*, size_t);
  void *userdata;
public:
  net(void (*n_receive_cb)(void*, uint8_t*, size_t), void *n_userdata);
  void send(uint8_t *message, size_t len);
  void poll();
  void set_endpoint(std::string hostname);
};

enum class message_type : uint8_t {
  ACK = 0,
  PING = 1,
  CONNECTION_REQ = 2
};

enum class server_message_type : uint8_t {
  ACK = 0,
  PONG = 1,
  CONNECTION_REPLY = 2,
  ERROR = 3
};

enum class server_error_type : uint8_t {
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
  uint8_t  reliable; // 1 bits
  uint32_t sequence; // 31
  uint32_t ack_sequence;
  uint16_t client_id;
  uint8_t  num_messages;
  bytestream serialized_messages;
};

struct message {
  message_type type;
  virtual void serialize(bytestream &b) = 0;
};

struct ping_msg : message {
  void serialize(bytestream &b);
};

class connection {
  // std::queue<> reliable_messages_buffer;
  // std::queue<> sent_unacked_reliable_messages;
  std::queue<message*> unreliable_messages;
  uint16_t client_id;
  net *n;
  double ping_send_delay, internal_time_counter, time_since_last_pong;
  void ping();
public:
  connection();
  void update(double dt);
  void receive_pong();
  static void receive(void *userdata, uint8_t *buffer, size_t bytes_rx);
  void send();
};

