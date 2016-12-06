#pragma once
#include "bits.hh"
#include "screen.hh"
#include <asio.hpp>
#include <queue>
#include <thread>

static const int port_serv = 2711, port_client = 2710, max_msg_len = 256;

class net {
  asio::ip::udp::socket _socket;
  asio::ip::udp::endpoint _remote_endpoint;
  uint8_t _recv_buffer[max_msg_len];
  void (*receive_cb)(void*, uint8_t*, size_t);
  void *userdata;
  void start_receive();
public:
  net(asio::io_service &io, void (*n_receive_cb)(void*, uint8_t*, size_t)
      , void *n_userdata);
  void send(uint8_t *message, size_t len);
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

enum class connection_state_type : uint8_t {
  disconnected,
  connecting,
  connection_failed,
  connected
};

struct packet_header {
  uint8_t  reliable; // 1 bits
  uint32_t sequence; // 31
  uint32_t ack_sequence;
  uint16_t client_id;
  uint8_t  num_messages;
  bytestream serialized_messages;
  void serialize(bytestream &b);
};

struct message {
  message_type type;
  virtual void serialize(bytestream &b) = 0;
};

struct ping_msg : message {
  uint32_t time_sent_ms;
  void serialize(bytestream &b);
};

struct connection_req_msg : message {
  uint16_t protocol_ver;
  void serialize(bytestream &b);
};

class connection {
  asio::io_service _io;
  std::thread _net_io_thread;
  net _n;
  bytestream unacked_reliable_messages;
  std::queue<bytestream> reliable_messages;
  std::queue<bytestream> unreliable_messages;
  uint32_t outgoing_sequence;
  uint16_t client_id;
  double ping_send_delay_ms, ping_time_counter_ms, time_since_last_pong;
  connection_state_type connection_state;

  screen *s; // for getting time

  void ping();
  void send_connection_req();
public:
  connection(screen *n_s);
  ~connection();
  void update(double dt, double t);
  void receive_pong(uint32_t time_sent_ms);
  static void receive(void *userdata, uint8_t *buffer, size_t bytes_rx);
  void send();
};

