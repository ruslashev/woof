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
public:
  net(asio::io_service &io, void (*n_receive_cb)(void*, uint8_t*, size_t)
      , void *n_userdata, int port);
  void start_receive();
  void send(uint8_t *message, size_t len);
  void set_endpoint(std::string hostname, int port);
};

const uint16_t protocol_version = 1;

enum class message_type : uint8_t {
  SPECIAL = 0,
  ISALIVE = 1,
  CONNECTION_REQ = 2
};

enum class server_message_type : uint8_t {
  SPECIAL = 0,
  CONNECTION_REPLY = 1,
  ERROR = 2
};

enum class server_error_type : uint8_t {
  NOT_MATCHING_PROTOCOL = 0
};

struct packet {
  uint8_t  reliable;
  uint32_t sequence;
  uint32_t ack;
  uint16_t client_id;
  uint8_t  num_messages;
  bytestream serialized_messages;
  void serialize(bytestream &b);
  void deserialize(bytestream &b, bool &success);
};

struct message {
  message_type type;
  message(message_type n_type);
  virtual void serialize(bytestream &b) = 0;
};

struct connection_req_msg : message {
  uint16_t protocol_ver;
  connection_req_msg();
  void serialize(bytestream &b) override;
};

class connection {
  asio::io_service _io;
  net _n;
  std::thread _net_io_thread;

  std::queue<bytestream> _unrel_messages, _messages;
  bool _unacked_packet_exists;
  packet _unacked_packet;
  uint32_t _outgoing_sequence, _last_sequence_received;
  uint64_t _sent_packets, _ack_packets, _received_packets;

  uint16_t _client_id;

  double _ping_send_delay_ms, _ping_time_counter_ms, _time_since_last_pong;

  screen *_s; // for getting time

  bool _connected;

  void ping();
public:
  connection(int port, screen *n_s);
  ~connection();
  void update(double dt, double t);
  static void receive(void *userdata, uint8_t *buffer, size_t bytes_rx);
  void send(bytestream msg);
  void send_rel(bytestream msg);
  void set_endpoint(std::string remote_ip, int remote_port);
  void test();
  void print_stats();
};

