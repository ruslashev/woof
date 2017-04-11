#include "net.hh"
#include "utils.hh"
#include <limits> // std::numeric_limits

net::net(void (*n_receive_cb)(void*, uint8_t*, size_t), void *n_userdata, int port)
try
  : _io()
  , _io_work(_io)
  , _io_thread([&] {
      puts("net thread start");
      start_receive();
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
  , _socket(_io, asio::ip::udp::endpoint(asio::ip::udp::v4(), port))
  , _receive_cb(n_receive_cb)
  , _userdata(n_userdata) {
  puts("net init");
} catch (const std::exception &e) {
  die("net init fail: %s", e.what());
} catch (...) {
  die("net init fail");
}

net::~net() {
  _io.stop();
  _io_thread.join();
}

void net::start_receive() {
  asio::ip::udp::endpoint remote_endpoint;
  _socket.async_receive_from(asio::buffer(_recv_buffer), remote_endpoint
      , [this, &remote_endpoint](const asio::error_code &e, size_t bytes_rx) {
        // printf("receive from %s:%d\n"
        //     , remote_endpoint.address().to_string().c_str()
        //     , remote_endpoint.port());
        _handle_receive(e, bytes_rx);
      });
}

void net::_handle_receive(const asio::error_code &e, size_t bytes_rx) {
  if (e == asio::error::message_size)
    warning_ln("received message longer than the buffer");
  else if (e)
    warning_ln("socket receive error: %d: %s", e.value(), e.message().c_str());
  else {
    _receive_cb(_userdata, _recv_buffer, bytes_rx);
  }
  start_receive();
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
  success &= b.read_rest(serialized_messages);
}

void packet::print() {
  printf("reliable=%d\n", reliable);
  printf("sequence=%d\n", sequence);
  printf("ack=%d\n", ack);
  printf("client_id=%d\n", client_id);
  printf("num_messages=%d: ", num_messages);
  serialized_messages.print();
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

ping_msg::ping_msg(uint32_t n_time_sent)
  : message(message_type::PING)
  , time_sent(n_time_sent) {
}

void ping_msg::serialize(bytestream &b) {
  b.write_uint8((uint8_t)type);
  b.write_uint32_net(time_sent);
}

void connection::_ping() {
  ping_msg m(_s->get_time_in_seconds() * 1000. + 0.5);
  bytestream b;
  m.serialize(b);
  send(b);
}

void connection::_pong(uint32_t time_sent) {
  _time_since_last_pong = 0;
  _connection_stalling_warned = false;
  double rtt = _s->get_time_in_seconds() * 1000. - (double)time_sent;
  printf("ping rtt %.1f\n", rtt);
}

void connection::_parse_messages(packet &p) {
  int num_messages = p.num_messages;
  bytestream messages = p.serialized_messages;
  while (num_messages != 0) {
    uint8_t type;
    messages.read_uint8(type);
    switch ((server_message_type)type) {
      case server_message_type::PONG:
        uint32_t time_sent;
        messages.read_uint32_net(time_sent);
        _pong(time_sent);
        break;
      case server_message_type::CONNECTION_REPLY:
        puts("connection established");
        _connected = true;
        break;
      default:
        warning("unknown message type received: %d", type);
    }
    num_messages--;
  }
}

connection::connection(int port, screen *n_s)
  : _n(receive, this, port)
  , _unacked_packet_exists(false)
  , _outgoing_sequence(0)
  , _last_sequence_received(0)
  , _sent_packets(0)
  , _ack_packets(0)
  , _received_packets(0)
  , _ping_send_delay_ms(1500)
  , _ping_time_counter_ms(0)
  , _time_since_last_pong(0)
  , _connection_stalling_warned(false)
  , _s(n_s)
  , _connected(false) {
  srand(time(nullptr));
  _client_id = 0;
  while (_client_id == 0)
    _client_id = rand();
  printf("this client id is %d\n", _client_id);
}

void connection::update(double dt, double t) {
  if (_connected) {
    _time_since_last_pong += dt;
    if (_time_since_last_pong > 3 && !_connection_stalling_warned) {
      warning("connection stalling");
      _connection_stalling_warned = true;
    } else if (_time_since_last_pong > 5)
      {} // die("connection timeout");
    _ping_time_counter_ms += dt * 1000.;
    if (_ping_time_counter_ms > _ping_send_delay_ms) {
      _ping();
      _ping_time_counter_ms -= _ping_send_delay_ms;
    }
  }

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
  if (!success) {
    warning("malformed packet");
    return;
  }
  ++c->_received_packets;
  if (!(p.sequence == 0 && c->_last_sequence_received == 0))
    if (p.sequence != c->_last_sequence_received + 1)
      return;
  c->_last_sequence_received = p.sequence;
  if (p.ack == c->_unacked_packet.sequence) {
    c->_unacked_packet_exists = false;
    ++c->_ack_packets;
  }
  c->_parse_messages(p);
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

void connection::connect() {
  // todo check if endpoint is set
  connection_req_msg m;
  bytestream b;
  m.serialize(b);
  send_rel(b);
}

void connection::print_stats() {
  printf("stats: sent=%lu, ack=%lu, received=%lu\n", _sent_packets, _ack_packets
      , _received_packets);
}

bool connection::is_connected() {
  return _connected;
}

