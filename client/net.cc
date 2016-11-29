#include "net.hh"

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

net::net(void (*n_receive_cb)(void*, uint8_t*, size_t)
    , void *n_userdata = nullptr)
  : _io()
  , _socket(_io, asio::ip::udp::endpoint(asio::ip::udp::v4(), port_client))
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

void net::poll() {
  _io.poll();
}

void net::set_endpoint(std::string hostname) {
  _remote_endpoint = asio::ip::udp::endpoint(
      asio::ip::address::from_string(hostname), port_serv);
}

void packet_header::serialize(bytestream &b) {
  b.write_uint32(((reliable & 1) << 31) | sequence);
  b.write_uint32(ack_sequence);
  b.write_uint16(client_id);
  b.write_uint8(num_messages);
  b.append(serialized_messages);
}

void ping_msg::serialize(bytestream &b) {
  b.write_uint8((uint8_t)type);
  b.write_uint32(htonl(time_sent));
}

void connection_req_msg::serialize(bytestream &b) {
  b.write_uint8((uint8_t)type);
  b.write_uint16(htons(protocol_ver));
}

void connection::ping() {
  puts("ping");
  ping_msg p;
  p.type = message_type::PING;
  p.time_sent = t;
  bytestream b;
  p.serialize(b);
  unreliable_messages.push(b);
}

void connection::send_connection_req() {
  connection_req_msg r;
  r.type = message_type::CONNECTION_REQ;
  r.protocol_ver = 1;
  bytestream b;
  r.serialize(b);
  reliable_messages.push(b);
}

connection::connection() : outgoing_sequence(1), ping_send_delay(1000)
    , internal_time_counter(0), time_since_last_pong(0)
    , connection_state(connection_state_type::disconnected) {
  srand(time(nullptr));
  client_id = rand();
  printf("this client id is %d\n", client_id);
  n = new net(receive, this);
};

void connection::update(double dt, uint32_t t) {
  this->t = t;
  if (unreliable_messages.size())
    printf("unreliable_messages: %zu\n", unreliable_messages.size());
  packet_header packet;
  packet.reliable = 0;
  packet.sequence = outgoing_sequence++;
  packet.ack_sequence = 0;
  packet.client_id = client_id;
  packet.num_messages = 0;
  if (unreliable_messages.size() >= 64)
    die("message buffer overflow");
  if (reliable_messages.size() >= 64)
    die("rel. message buffer overflow");
  for (size_t i = 0; unreliable_messages.size()
      && i < unreliable_messages.size(); i++) {
    packet.serialized_messages.append(unreliable_messages.front());
    ++packet.num_messages;
    unreliable_messages.pop();
  }
  if (unacked_reliable_messages.empty()) {
    for (size_t i = 0; reliable_messages.size()
        && i < reliable_messages.size(); i++) {
      packet.reliable = 1;
      unacked_reliable_messages.append(reliable_messages.front());
      ++packet.num_messages;
      reliable_messages.pop();
    }
    packet.serialized_messages.append(unacked_reliable_messages);
  }
  if (packet.num_messages) {
    printf("reliable: %d, ", packet.reliable);
    packet.serialized_messages.print("new packet");
    bytestream pb;
    packet.serialize(pb);
    n->send(pb.get_data(), pb.get_size());
  }

  internal_time_counter += dt * 1000.0;
  if (internal_time_counter > ping_send_delay) {
    internal_time_counter -= ping_send_delay;
    ping();
  }
  n->poll();
}

void connection::receive_pong(uint32_t time_sent) {
  time_since_last_pong = 0;
  printf("time_sent: %d\n", time_sent);
  printf("rtt: %d\n", t - time_sent);
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

