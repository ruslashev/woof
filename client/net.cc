#include "net.hh"
#include "utils.hh"
#include <limits> // std::numeric_limits

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

net::net(asio::io_service &io, void (*n_receive_cb)(void*, uint8_t*, size_t)
    , void *n_userdata = nullptr)
  : _socket(io, asio::ip::udp::endpoint(asio::ip::udp::v4(), port_client))
  , receive_cb(n_receive_cb)
  , userdata(n_userdata) {
  set_endpoint("127.0.0.1");
  start_receive();
}

void net::send(uint8_t *message, size_t len) {
  _socket.async_send_to(asio::buffer(message, len), _remote_endpoint
      , [this, len](const asio::error_code &e, size_t bytes_tx) {
        if (bytes_tx != len)
          printf("somewhy %zu/%zu bytes have been sent\n", bytes_tx, len);
        if (e && e != asio::error::message_size)
          puts("some kind of error happened on sending");
        else
          puts("ok, sent");
      });
}

void net::set_endpoint(std::string hostname) {
  _remote_endpoint = asio::ip::udp::endpoint(
      asio::ip::address::from_string(hostname), port_serv);
}

void packet_header::serialize(bytestream &b) {
  b.write_uint32(((reliable & 1) << 31) | sequence);
  b.write_uint32(ack);
  b.write_uint16(client_id);
  b.write_uint8(num_messages);
  b.append(serialized_messages);
}

message::message(message_type n_type) : type(n_type) {
}

ping_msg::ping_msg() : message(message_type::PING)
  , time_sent_ms(std::numeric_limits<uint32_t>::max()) {
}

void ping_msg::serialize(bytestream &b) {
  b.write_uint8((uint8_t)type);
  b.write_uint32(htonl(time_sent_ms));
}

connection_req_msg::connection_req_msg() : message(message_type::CONNECTION_REQ)
  , protocol_ver(1) {
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

void connection::send_connection_req() {
  connection_req_msg r;
  bytestream b;
  r.serialize(b);
  _reliable_messages.push(b);
}

connection::connection(screen *n_s)
  : _io()
  , _net_io_thread([&] {
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
  , _n(_io, receive, this)
  , _outgoing_sequence(1)
  , _ping_send_delay_ms(1500)
  , _ping_time_counter_ms(0)
  , _time_since_last_pong(0)
  , _connection_state(connection_state_type::disconnected)
  , _s(n_s) {
  srand(time(nullptr));
  _client_id = rand();
  printf("this client id is %d\n", _client_id);
}

connection::~connection() {
  _io.stop();
  _net_io_thread.join();
}

void connection::update(double dt, double t) {
  _time_since_last_pong += dt;
  if (_time_since_last_pong > 5)
    die("connection timeout");
  _ping_time_counter_ms += dt * 1000.;
  if (_ping_time_counter_ms > _ping_send_delay_ms) {
    ping();
    _ping_time_counter_ms -= _ping_send_delay_ms;
  }

  if (_unreliable_messages.size() >= 64)
    die("message buffer overflow");
  if (_reliable_messages.size() >= 64)
    die("rel. message buffer overflow");
  if (_unreliable_messages.size() + _reliable_messages.size()
      + _unacked_reliable_messages.size()) {
    packet_header packet;
    packet.reliable = 0;
    packet.sequence = _outgoing_sequence++;
    packet.ack = 0;
    packet.client_id = _client_id;
    packet.num_messages = 0;
    for (size_t i = 0; _unreliable_messages.size()
        && i < _unreliable_messages.size(); i++) {
      packet.serialized_messages.append(_unreliable_messages.front());
      ++packet.num_messages;
      _unreliable_messages.pop();
    }
    if (_unacked_reliable_messages.empty()) {
      if (_reliable_messages.size()) {
        for (size_t i = 0; i < _reliable_messages.size(); i++) {
          packet.reliable = 1;
          _unacked_reliable_messages.append(_reliable_messages.front());
          ++packet.num_messages;
          _reliable_messages.pop();
        }
        packet.serialized_messages.append(_unacked_reliable_messages);
      }
    } else {
      packet.reliable = 1;
      packet.serialized_messages.append(_unacked_reliable_messages);
    }
    printf("reliable: %d, ", packet.reliable);
    packet.serialized_messages.print("new packet");
    bytestream pb;
    packet.serialize(pb);
    _n.send(pb.data(), pb.size());
  }
}

void connection::receive_pong(uint32_t time_sent_ms) {
  _time_since_last_pong = 0;
  uint32_t curr_ms = std::lround(_s->get_time_in_seconds() * 1000.);
  printf("rtt: %d ms\n", curr_ms - time_sent_ms);
}

void connection::receive(void *userdata, uint8_t *buffer, size_t bytes_rx) {
  connection *c = (connection*)userdata;
  print_packet(buffer, bytes_rx, "received packet");
  switch (buffer[0]) {
    case (uint8_t)server_message_type::ACK:
      puts("type: ACK");
      break;
    case (uint8_t)server_message_type::PONG:
      puts("type: PONG");
      c->receive_pong((*((uint32_t*)(buffer + 1))));
      break;
    case (uint8_t)server_message_type::CONNECTION_REPLY:
      puts("type: CONNECTION_REPLY");
      // net_state.client_id = ntohs(*(uint16_t*)(buffer + 1));
      // printf("assigned client id: %d\n", net_state.client_id);
      break;
    case (uint8_t)server_message_type::ERROR:
      puts("type: ERROR");
      switch (buffer[1]) {
        case (uint8_t)server_error_type::NOT_MATCHING_PROTOCOL:
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

void connection::send() {

}

