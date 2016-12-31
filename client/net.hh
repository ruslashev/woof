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
      , void *n_userdata);
  void start_receive();
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
  uint16_t client_id;
  uint16_t sequence;
  uint16_t ack;
  uint32_t ack_bits;
  uint8_t  num_messages;
  bytestream serialized_messages;
  void serialize(bytestream &b);
};

struct message {
  message_type type;
  message(message_type n_type);
  virtual void serialize(bytestream &b) = 0;
};

struct ping_msg : message {
  ping_msg();
  void serialize(bytestream &b) override;
};

struct connection_req_msg : message {
  uint16_t protocol_ver;
  connection_req_msg();
  void serialize(bytestream &b) override;
};

const uint16_t protocol_version = 1;

const int sequence_buffer_size = 1024;
struct packet_data {
  bool acked;
  uint32_t time_sent_ms;
  packet_data();
};

class connection {
  asio::io_service _io;
  std::thread _net_io_thread;
  net _n;

  uint32_t _sequence_buffer[sequence_buffer_size];
  packet_data _packet_data[sequence_buffer_size];

  std::queue<bytestream> _reliable_messages, _unreliable_messages;
  bytestream _unacked_reliable_messages;

  uint32_t _outgoing_sequence, _last_sequence_received;
  uint16_t _client_id;
  double _ping_send_delay_ms, _ping_time_counter_ms, _time_since_last_pong;
  connection_state_type _connection_state;

  screen *_s; // for getting time

  bool _connected;

  packet_data* get_packet_data(uint16_t sequence);
  packet_data& insert_packet_data(uint16_t sequence);
  void ping();
public:
  connection(screen *n_s);
  ~connection();
  void update(double dt, double t);
  void receive_pong(uint32_t time_sent_ms);
  static void receive(void *userdata, uint8_t *buffer, size_t bytes_rx);
  void send();
  void connect(std::string remote_ip);
};

