#include "net.hh"

static void print_packet(uint8_t *packet, size_t len
    , const char *msg = "packet") {
  printf("%s: ", msg);
  for (size_t i = 0; i < len; i++) {
    int numbits = 8;
    while (--numbits >= 0)
      printf("%c", (packet[i] & ((uint8_t)1 << numbits)) ? '1' : '0');
    printf(" ");
  }
  printf("\n");
}

void net::start_receive() {
  _socket.async_receive_from(asio::buffer(_recv_buffer), _remote_endpoint
      , [this](const asio::error_code &e, size_t bytes_rx) {
        if (e == asio::error::message_size)
          warning("received message longer than the buffer");
        else if (e || bytes_rx == 0)
          warning("something is wrong");
        else {
          printf("receive from %s:%d\n"
              , _remote_endpoint.address().to_string().c_str()
              , _remote_endpoint.port());
          print_packet(_recv_buffer, bytes_rx, "received packet");
          receive_cb(_recv_buffer, bytes_rx);
        }
        start_receive();
      });
}

net::net(void (*n_receive_cb)(uint8_t*, size_t))
  : _io()
  , _socket(_io, asio::ip::udp::endpoint(asio::ip::udp::v4(), port_client))
  , receive_cb(n_receive_cb) {
    start_receive();
}

void net::send(uint8_t *message, size_t len) {
  asio::ip::udp::endpoint recv_endpoint(
      asio::ip::address::from_string("127.0.0.1"), port_serv);
  _socket.async_send_to(asio::buffer(message, len), recv_endpoint
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

static uint32_t generate_rel_msg_id() {
  static uint32_t id = 0;
  ++id;
  return id;
}

void send_connection_req(net *n) {
  /* type         : 7 bits  = CONNECTION_REQ
   * reliable     : 1 bit   = 1
   * rel_msg_id   : 32 bits = ...
   * protocol_ver : 8 bits  = 1
   */
  uint8_t connection_req_packet_serialized[6] = { 0b00000011 };
  uint32_t rel_msg_id = htonl(generate_rel_msg_id());
  *(uint32_t*)((uint8_t*)connection_req_packet_serialized + 1) = rel_msg_id;
  *(uint8_t*)(connection_req_packet_serialized + 5) = 1;
  print_packet(connection_req_packet_serialized, 6);
  n->send(connection_req_packet_serialized, 6);
}

