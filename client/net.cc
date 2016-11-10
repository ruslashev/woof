#include "net.hh"

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
          receive_cb(_recv_buffer, bytes_rx);
        }
        start_receive();
      });
}

net::net(void (*n_receive_cb)(char*, size_t))
  : _io()
  , _socket(_io, asio::ip::udp::endpoint(asio::ip::udp::v4(), port_client))
  , receive_cb(n_receive_cb) {
    start_receive();
}

void net::send(char *message, size_t len) {
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

void send_connection_req(net *n) {
  /* reliable     : 1 bit  = 1
   * type         : 4 bits = CONNECTION_REQ
   * protocol_ver : 3 bits = 1
   */
  char connection_req_packet_serialized_to_byte =
    0b10001001;
  n->send(&connection_req_packet_serialized_to_byte, 1);
}

