#include "net.hh"

static uint32_t generate_sequence_number() {
  static uint32_t seq = 0;
  ++seq;
  return seq;
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

void ping_msg::serialize(bytestream &b) {
  b.write_uint8((uint8_t)type);
}

void connection::ping() {
  puts("ping");
  ping_msg *p = new ping_msg;
  p->type = message_type::PING;
  unreliable_messages.push(p);
}

connection::connection() : ping_send_delay(1000), internal_time_counter(0)
    , time_since_last_pong(0) {
  srand(time(nullptr));
  client_id = rand();
  printf("this client id is %d\n", client_id);
  n = new net(receive, this);
};

void connection::update(double dt) {
  if (unreliable_messages.size())
    printf("unreliable_messages: %zu\n", unreliable_messages.size());
  packet_header packet;
  packet.reliable = 0;
  packet.sequence = generate_sequence_number();
  packet.ack_sequence = 0;
  packet.client_id = client_id;
  packet.num_messages = 0;
  for (size_t i = 0; unreliable_messages.size() && i < 5
      && i < unreliable_messages.size(); i++) {
    puts("putting message in the packet");
    message *m = unreliable_messages.front();
    ++packet.num_messages;
    m->serialize(packet.serialized_messages);
    unreliable_messages.pop();
    // delete m;
  }
  if (packet.num_messages)
    packet.serialized_messages.print("new packet is ready: ");

  internal_time_counter += dt * 1000.0;
  if (internal_time_counter > ping_send_delay) {
    internal_time_counter -= ping_send_delay;
    ping();
  }
  n->poll();
}

void connection::receive_pong() {
  time_since_last_pong = 0;
}

void connection::receive(void *userdata, uint8_t *buffer, size_t bytes_rx) {
  connection *c = (connection*)userdata;
  print_packet(buffer, bytes_rx, "received packet");
  switch ((buffer[0] & 0b11111110) >> 1) {
    case (uint8_t)server_message_type::ACK:
      puts("type: ACK");
      break;
    case (uint8_t)server_message_type::PONG:
      puts("type: PONG");
      c->receive_pong();
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

