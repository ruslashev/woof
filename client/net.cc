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
          printf("receive from %s:%d\n"
              , remote_endpoint.address().to_string().c_str()
              , remote_endpoint.port());
          receive_cb(userdata, _recv_buffer, bytes_rx);
        }
        start_receive();
      });
}

void net::send(uint8_t *message, size_t len) {
  _socket.async_send_to(asio::buffer(message, len), _remote_endpoint
      , [this](const asio::error_code &e, size_t bytes_tx) {
        if (e)
          puts("some kind of error happened on sending");
        else
          puts("ok, sent");
      });
}

void net::set_endpoint(std::string hostname, int port) {
  _remote_endpoint = asio::ip::udp::endpoint(
      asio::ip::address::from_string(hostname), port);
}

void packet::serialize(bytestream &b) {
  b.write_uint16(client_id);
  b.write_uint8(reliable);
  b.write_uint32(sequence);
  b.write_uint32(ack);
  b.write_uint8(num_messages);
  b.append(serialized_messages);
}

void packet::deserialize(bytestream &b, bool &success) {
  success |= b.read_uint16(client_id);
  success |= b.read_uint8(reliable);
  success |= b.read_uint32(sequence);
  success |= b.read_uint32(ack);
  success |= b.read_uint8(num_messages);
}

message::message(message_type n_type)
  : type(n_type) {
}

ping_msg::ping_msg()
  : message(message_type::PING)
  , time_sent_ms(std::numeric_limits<uint32_t>::max()) {
}

void ping_msg::serialize(bytestream &b) {
  b.write_uint8((uint8_t)type);
  b.write_uint32(htonl(time_sent_ms));
}

connection_req_msg::connection_req_msg()
  : message(message_type::CONNECTION_REQ)
  , protocol_ver(protocol_version) {
}

void connection_req_msg::serialize(bytestream &b) {
  b.write_uint8((uint8_t)type);
  b.write_uint16(htons(protocol_ver));
}

void connection::ping() {
  ping_msg p;
  p.type = message_type::PING;
  p.time_sent_ms = std::lround(_s->get_time_in_seconds() * 1000.);
  bytestream b;
  p.serialize(b);
  _unreliable_messages.push(b);
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
  , _outgoing_sequence(0)
  , _last_sequence_received(0)
  , _sent_packets(0)
  , _received_packets(0)
  , _ping_send_delay_ms(1500)
  , _ping_time_counter_ms(0)
  , _time_since_last_pong(0)
  , _connection_state(connection_state_type::disconnected)
  , _s(n_s)
  , _connected(false) {
  srand(time(nullptr));
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
  if (_unreliable_messages.size() >= 64)
    die("message buffer overflow");
  if (_reliable_messages.size() >= 64)
    die("rel. message buffer overflow");

  if (!_unreliable_messages.empty() || !_reliable_messages.empty()
      || !_unacked_reliable_messages.empty()) {
    packet packet;
    packet.client_id = _client_id;
    packet.reliable = 0;
    packet.sequence = _outgoing_sequence++;
    packet.ack = _last_sequence_received;
    packet.num_messages = 0;

    if (!_unreliable_messages.empty())
      for (size_t i = 0; i < _unreliable_messages.size(); i++) {
        packet.serialized_messages.append(_unreliable_messages.front());
        ++packet.num_messages;
        _unreliable_messages.pop();
      }

    if (!_unacked_reliable_messages.empty()) {
      packet.reliable = 1;
      packet.serialized_messages.append(_unacked_reliable_messages);
    } else {
      if (!_reliable_messages.empty()) {
        packet.reliable = 1;
        bytestream rel_messages;
        for (size_t i = 0; i < _reliable_messages.size(); i++) {
          _unacked_reliable_messages.append(_reliable_messages.front());
          ++packet.num_messages;
          _reliable_messages.pop();
        }
        packet.serialized_messages.append(_unacked_reliable_messages);
      }
    }

    packet.serialized_messages.print("new packet");
    bytestream b;
    packet.serialize(b);
    _n.send(b.data(), b.size());
    ++_sent_packets;
    print_stats();
  }
}

void connection::receive(void *userdata, uint8_t *buffer, size_t bytes_rx) {
  connection *c = (connection*)userdata;
  print_packet(buffer, bytes_rx, "received packet");
  ++c->_received_packets;
  c->print_stats();
}

void connection::send(bytestream msg) {
  _unreliable_messages.push(msg);
}

void connection::send_rel(bytestream msg) {
  _reliable_messages.push(msg);
}

void connection::test(std::string remote_ip, int remote_port) {
  _n.set_endpoint(remote_ip, remote_port);

  std::string text = "hi";
  bytestream msg;
  for (size_t i = 0; i < text.size(); ++i)
    msg.write_uint8(text[i]);
  send(msg);
}

void connection::print_stats() {
  printf("_sent_packets=%llu, _received_packets=%llu\n", _sent_packets
      , _received_packets);
}

