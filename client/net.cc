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

void packet_header::serialize(bytestream &b) {
  b.write_net(client_id);
  b.write_net(sequence);
  b.write_net(ack);
  b.write_net(ack_bits);
  b.write_net(num_messages);
  b.append(serialized_messages);
}

bool packet_header::deserialize(bytestream &b) {
  bool success = true;
  success &= b.read_net(client_id);
  success &= b.read_net(sequence);
  success &= b.read_net(ack);
  success &= b.read_net(ack_bits);
  success &= b.read_net(num_messages);
  return success;
}

message::message(message_type n_type)
  : type(n_type) {
}

ping_msg::ping_msg()
  : message(message_type::PING) {
}

void ping_msg::serialize(bytestream &b) {
  b.write_net((uint8_t)type);
}

connection_req_msg::connection_req_msg()
  : message(message_type::CONNECTION_REQ)
  , protocol_ver(protocol_version) {
}

void connection_req_msg::serialize(bytestream &b) {
  b.write_net((uint8_t)type);
  b.write_net(htons(protocol_ver));
}

packet_data::packet_data()
  : sequence(0)
  , time_sent_ms(std::numeric_limits<uint32_t>::max()) {
}

inline bool sequence_more_recent(uint16_t s1, uint16_t s2) {
  const uint16_t max_sequence = std::numeric_limits<uint16_t>::max();
  return ((s1 > s2) && (s1 - s2 <= max_sequence / 2)) || ((s2 > s1)
      && (s2 - s1 > max_sequence / 2));
}

/*
packet_data* connection::get_packet_data(uint16_t sequence) {
  const int idx = sequence % sequence_buffer_size;
  if (_sequence_buffer[idx] == sequence)
    return &_packet_data[idx];
  else
    return nullptr;
}

packet_data& connection::insert_packet_data(uint16_t sequence) {
  const int idx = sequence % sequence_buffer_size;
  _sequence_buffer[idx] = sequence;
  return _packet_data[idx];
}
*/

/*
void connection::ping() {
  ping_msg p;
  p.type = message_type::PING;
  bytestream b;
  p.serialize(b);
  _unreliable_messages.push(b);
}
*/

int connection::bit_idx_for_sequence(uint16_t sequence, uint16_t ack) {
  assertf(sequence != ack, "unexpected sequence = ack = %d", sequence);
  assertf(!sequence_more_recent(sequence, ack), "unexpected seq and ack values");
  if (sequence > ack) {
    assertf(ack < 33, "ack = %d", ack);
    return ack + (std::numeric_limits<uint16_t>::max() - sequence);
  } else {
    assertf(sequence <= ack - 1, "unexpected seq and ack values");
    return ack - 1 - sequence;
  }
}

uint32_t connection::generate_ack_bits() {
  uint32_t ack_bits = 0;
  for (const packet_data &d : _received_queue) {
    if (d.sequence == _last_sequence_received
        || sequence_more_recent(d.sequence, _last_sequence_received))
      break;
    int bit_index = bit_idx_for_sequence(d.sequence, _last_sequence_received);
    if (bit_index <= 31)
      ack_bits |= 1 << bit_index;
  }
  return ack_bits;
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
  , _sent_packets(0)
  , _lost_packets(0)
  , _received_packets(0)
  , _acked_packets(0)
  , _outgoing_sequence(0)
  , _last_sequence_received(0)
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
  const int max_rtt = 300;
  _acks.clear();

  while (_sent_pq.size() &&
      _s->get_time_in_seconds() * 1000. - _sent_pq.front().time_sent_ms > max_rtt) {
    // printf("dropping packet %d from sent_pq after timeout\n"
    //     , _sent_pq.front().sequence);
    _sent_pq.pop_front();
  }

  if (_received_pq.size()) {
    const uint16_t latest_sequence = _received_pq.back().sequence
      , max_sequence = std::numeric_limits<uint16_t>::max()
      , min_sequence = latest_sequence >= 34
        ? (latest_sequence - 34)
        : max_sequence - (34 - latest_sequence);
    while (_received_pq.size()
        && !sequence_more_recent(_received_pq.front().sequence, min_sequence))
      _received_queue.pop_front();
  }

  while (_acked_pq.size() && _s->get_time_in_seconds() * 1000.
      - _acked_pq.front().time_sent_ms > max_rtt * 2) {
    // printf("dropping packet %d from acked_pq after timeout\n"
    //     , _acked_pq.front().sequence);
    _acked_pq.pop_front();
  }

  while (_pending_ack_pq.size() && _s->get_time_in_seconds() * 1000.
      - _pending_ack_pq.front().time_sent_ms > max_rtt) {
    // printf("dropping packet %d from pending_ack_pq after timeout\n"
    //     , _pending_ack_pq.front().sequence);
    _pending_ack_pq.pop_front();
    ++_lost_packets;
  }

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

  /*
  if (_unreliable_messages.size() >= 64)
    die("message buffer overflow");
  if (_reliable_messages.size() >= 64)
    die("rel. message buffer overflow");
  if (_unreliable_messages.size() + _reliable_messages.size()
      + _unacked_reliable_messages.size()) {
    packet_header packet;
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
          _unacked_reliable_messages.append(_reliable_messages.front());
          ++packet.num_messages;
          _reliable_messages.pop();
        }
        packet.serialized_messages.append(_unacked_reliable_messages);
      }
    } else {
      packet.serialized_messages.append(_unacked_reliable_messages);
    }
    // printf("reliable: %d, ", packet.reliable);
    packet.serialized_messages.print("new packet");
    bytestream pb;
    packet.serialize(pb);
    _n.send(pb.data(), pb.size());
  }
  */
}

/*
void connection::receive_pong(uint32_t time_sent_ms) {
  _time_since_last_pong = 0;
  uint32_t curr_ms = std::lround(_s->get_time_in_seconds() * 1000.);
  printf("rtt: %d ms\n", curr_ms - time_sent_ms);
}
*/

void connection::receive(void *userdata, uint8_t *buffer, size_t bytes_rx) {
  connection *c = (connection*)userdata;
  print_packet(buffer, bytes_rx, "received packet");
  bytestream packet(buffer, bytes_rx);
  packet_header header;
  if (!header.deserialize(packet))
    warning("malformed packet");

  ++c->_received_packets;
  if (c->_received_pq.exists(header.sequence))
    return;
  packet_data d;
  d.sequence = header.sequence;
  d.time_sent_ms = 0.0f;
  c->_received_queue.push_back(d);
  if (sequence_more_recent(header.sequence, c->_last_sequence_received))
    c->_last_sequence_received = header.sequence;

  if (c->_pending_ack_pq.empty())
    return;
  packet_queue::iterator i = c->_pending_ack_pq.begin();
  while (i != c->_pending_ack_pq.end()) {
    bool acked = false;
    if (i->sequence == header.ack)
      acked = true;
    else if (!sequence_more_recent(i->sequence, header.ack)) {
      int bit_index = c->bit_idx_for_sequence(i->sequence, header.ack);
      if (bit_index <= 31)
        acked = (header.ack_bits >> bit_index) & 1;
    }
    if (acked) {
      // rtt += (i->time_sent_ms - rtt) * 0.1f;
      c->_acked_pq.insert_sorted(*i);
      c->_acks.push_back(i->sequence);
      ++c->_acked_packets;
      i = c->_pending_ack_pq.erase(i);
    } else
      ++i;
  }
}

void connection::send(const bytestream &message) {
  packet_header packet;
  packet.client_id = _client_id;
  packet.sequence = _outgoing_sequence;
  packet.ack = _last_sequence_received;
  packet.ack_bits = generate_ack_bits();
  packet.num_messages = 1;
  packet.serialized_messages = message;

  bytestream serialized_packet;
  packet.serialize(serialized_packet);
  // print_packet(serialized_packet.data(), serialized_packet.size(), "sending:");
  _n.send(serialized_packet.data(), serialized_packet.size());

  if (_sent_pq.exists(_outgoing_sequence)) {
    printf("local sequence %d exists\n", _outgoing_sequence);
    for (const packet_data &d : _sent_pq)
      printf(" + %d\n", d.sequence);
  }
  assertf(!_sent_pq.exists(_outgoing_sequence)
      , "local sequence exists in sent_pq");
  assertf(!_pending_ack_pq.exists(_outgoing_sequence)
      , "local sequence exists in pending_ack_pq");
  packet_data d;
  d.sequence = _outgoing_sequence;
  d.time_sent_ms = _s->get_time_in_seconds() * 1000.;
  _sent_pq.push_back(d);
  _pending_ack_pq.push_back(d);
  ++_sent_packets;
  ++_outgoing_sequence;
}

void connection::connect(std::string remote_ip, int remote_port) {
  _n.set_endpoint(remote_ip, remote_port);
  std::string text = "ohai";
  bytestream msg;
  for (size_t j = 0; j < text.size(); ++j)
    msg.write_net((uint8_t)text[j]);
  send(msg);
}

void connection::print_stats() {
  printf("sent=%llu, lost=%llu, received=%llu, acked=%llu\n", _sent_packets
      , _lost_packets, _received_packets, _acked_packets);
}

