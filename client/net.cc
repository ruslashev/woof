#include "net.hh"

void net::start_receive() {
  _socket.async_receive_from(asio::buffer(_recv_buffer), _remote_endpoint
      , [this](const asio::error_code &e, size_t bytes_rx) {
        if (e == asio::error::message_size)
          warning("received message longer than the buffer");
        else if (e || bytes_rx == 0)
          warning("something is wrong");
        else
          receive_cb(_recv_buffer, bytes_rx);
        start_receive();
      });
}

net::net(void (*n_receive_cb)(char*, size_t))
  : _io()
  , _socket(_io, asio::ip::udp::endpoint(asio::ip::udp::v4(), _port))
  , receive_cb(n_receive_cb) {
    start_receive();
}

void net::poll() {
  _io.poll();
}

