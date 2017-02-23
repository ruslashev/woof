#include "net.hh"
#include "utils.hh"
#include <limits> // std::numeric_limits

net::net(asio::io_service &io, void (*n_receive_cb)(void*, uint8_t*, size_t)
    , void *n_userdata, int port)
  try
  : _socket(io, asio::ip::udp::endpoint(asio::ip::udp::v4(), port))
  , receive_cb(n_receive_cb)
  , userdata(n_userdata) {
  start_receive();
} catch (const std::exception &e) {
  die("net init fail: %s", e.what());
} catch (...) {
  die("net init fail");
}

void net::start_receive() {
  // TODO: while (1) instead of recursion
  asio::ip::udp::endpoint remote_endpoint;
  _socket.async_receive_from(asio::buffer(_recv_buffer), remote_endpoint
      , [this, &remote_endpoint](const asio::error_code &e, size_t bytes_rx) {
        if (e == asio::error::message_size)
          warning("received message longer than the buffer");
        else if (e || bytes_rx == 0)
          warning("something is wrong");
        else {
          // printf("receive from %s:%d\n"
          //     , remote_endpoint.address().to_string().c_str()
          //     , remote_endpoint.port());
          receive_cb(userdata, _recv_buffer, bytes_rx);
        }
        start_receive();
      });
}

void net::send(uint8_t *message, size_t len) {
  _socket.async_send_to(asio::buffer(message, len), _remote_endpoint
      , [this](const asio::error_code &e, size_t bytes_tx) {
        if (e)
          printf("net::send error: %s\n", e.message().c_str());
        // else
        //   puts("ok, sent");
      });
}

void net::set_endpoint(std::string hostname, int port) {
  _remote_endpoint = asio::ip::udp::endpoint(
      asio::ip::address::from_string(hostname), port);
}

void packet::serialize(bytestream &b) {
  b.write_uint8(reliable);
  b.write_uint32_net(sequence);
  b.write_uint32_net(ack);
  b.write_uint16_net(client_id);
  b.write_uint8(num_messages);
  b.append(serialized_messages);
}

void packet::deserialize(bytestream &b, bool &success) {
  success  = b.read_uint8(reliable);
  success &= b.read_uint32_net(sequence);
  success &= b.read_uint32_net(ack);
  success &= b.read_uint16_net(client_id);
  success &= b.read_uint8(num_messages);
}

message::message(message_type n_type)
  : type(n_type) {
}

connection_req_msg::connection_req_msg()
  : message(message_type::CONNECTION_REQ)
  , protocol_ver(protocol_version) {
}

void connection_req_msg::serialize(bytestream &b) {
  b.write_uint8((uint8_t)type);
  b.write_uint16_net(protocol_ver);
}

void connection::ping() {
}

connection::connection(int port, screen *n_s)
  : _io()
  , _n(_io, receive, this, port)
  , _net_io_thread([&] {
      puts("net thread start");
      while (!_io.stopped()) {
        try {
          _io.run();
        } catch (const std::exception& e) {
          die("network exception: %s", e.what());
        } catch (...) {
          die("unknown network exception");
        }
      }
    })
  , _unacked_packet_exists(false)
  , _outgoing_sequence(0)
  , _last_sequence_received(0)
  , _sent_packets(0)
  , _ack_packets(0)
  , _received_packets(0)
  , _ping_send_delay_ms(1500)
  , _ping_time_counter_ms(0)
  , _time_since_last_pong(0)
  , _s(n_s)
  , _connected(false) {
  srand(time(nullptr));
  _client_id = 0;
  while (_client_id == 0)
    _client_id = rand();
  printf("this client id is %d\n", _client_id);
}

connection::~connection() {
  _io.stop();
  _net_io_thread.join();
}

void connection::update(double dt, double t) {
  /*
  if (_connected) {
    _time_since_last_pong += dt;
    if (_time_since_last_pong > 5)
      die("connection timeout");
    _ping_time_counter_ms += dt * 1000.;
    if (_ping_time_counter_ms > _ping_send_delay_ms) {
      ping();
      _ping_time_counter_ms -= _ping_send_delay_ms;
    }
  }
  */
  if (_unrel_messages.size() > 64)
    die("unrel message buffer overflow");
  if (_messages.size() > 64)
    die("message buffer overflow");

  if (_unacked_packet_exists || !_unrel_messages.empty() || !_messages.empty()) {
    bytestream serialized_packet;

    if (_unacked_packet_exists) {
      if (!_unrel_messages.empty())
        for (size_t i = 0; i < _unrel_messages.size(); ++i) {
          _unacked_packet.serialized_messages.append(_unrel_messages.front());
          ++_unacked_packet.num_messages;
          _unrel_messages.pop();
        }
      _unacked_packet.serialize(serialized_packet);
      printf("existing packet; ");
    } else if (!_messages.empty()) {
      packet new_packet;
      new_packet.reliable = 1;
      new_packet.sequence = _outgoing_sequence++;
      new_packet.ack = _last_sequence_received;
      new_packet.client_id = _client_id;
      new_packet.num_messages = 0;
      for (size_t i = 0; i < _messages.size(); ++i) {
        new_packet.serialized_messages.append(_messages.front());
        ++new_packet.num_messages;
        _messages.pop();
      }
      _unacked_packet_exists = true;
      _unacked_packet = new_packet;
      if (!_unrel_messages.empty())
        for (size_t i = 0; i < _unrel_messages.size(); ++i) {
          new_packet.serialized_messages.append(_unrel_messages.front());
          ++new_packet.num_messages;
          _unrel_messages.pop();
        }
      new_packet.serialize(serialized_packet);
      printf("new packet; ");
    } else if (!_unrel_messages.empty()) {
      packet new_packet;
      new_packet.reliable = 0;
      new_packet.sequence = _outgoing_sequence++;
      new_packet.ack = _last_sequence_received;
      new_packet.client_id = _client_id;
      new_packet.num_messages = 0;
      for (size_t i = 0; i < _unrel_messages.size(); ++i) {
        new_packet.serialized_messages.append(_unrel_messages.front());
        ++new_packet.num_messages;
        _unrel_messages.pop();
      }
      new_packet.serialize(serialized_packet);
      printf("new packet with unrel msgs only; ");
    }

    _n.send(serialized_packet.data(), serialized_packet.size());
    ++_sent_packets;
    print_stats();
  }
}

void connection::receive(void *userdata, uint8_t *buffer, size_t bytes_rx) {
  assertf(userdata != nullptr, "things that shouldn't happen for 300");
  connection *c = (connection*)userdata;
  // print_packet(buffer, bytes_rx, "receive");
  puts("received packet");
  bytestream packet_bs(buffer, bytes_rx);
  packet p;
  bool success;
  p.deserialize(packet_bs, success);
  if (!success)
    die("malformed packet");
  ++c->_received_packets;
  if (!(p.sequence == 0 && c->_last_sequence_received == 0))
    if (p.sequence != c->_last_sequence_received + 1)
      return;
  c->_last_sequence_received = p.sequence;
  if (c->_last_sequence_received == c->_unacked_packet.sequence) {
    c->_unacked_packet_exists = false;
    ++c->_ack_packets;
  }
  c->print_stats();
}

void connection::send(bytestream msg) {
  _unrel_messages.push(msg);
}

void connection::send_rel(bytestream msg) {
  _messages.push(msg);
}

void connection::set_endpoint(std::string remote_ip, int remote_port) {
  _n.set_endpoint(remote_ip, remote_port);
}

void connection::test() {
  std::string text = "hi";
  bytestream msg;
  for (size_t i = 0; i < text.size(); ++i)
    msg.write_uint8(text[i]);
  send(msg);
}

void connection::print_stats() {
  printf("stats: sent=%lu, ack=%lu, received=%lu\n", _sent_packets, _ack_packets
      , _received_packets);
}

