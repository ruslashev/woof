#pragma once
#include <asio.hpp>
#include "utils.hh"
#include <fstream>

class net {
  asio::io_service _io;
  asio::ip::udp::socket _socket;
  asio::ip::udp::endpoint _remote_endpoint;
  char _recv_buffer[1024];
  void start_receive() {
    _socket.async_receive_from(asio::buffer(_recv_buffer), _remote_endpoint
        , [this](const asio::error_code &e, size_t bytes_rx) {
          if (e == asio::error::message_size)
            warning("received message longer than the buffer");
          else if (e || bytes_rx == 0)
            warning("something is wrong");
          else {
            puts("receive:");
            for (size_t i = 0; i < bytes_rx; i++)
              printf("%c", _recv_buffer[i]);
            puts("");
          }
          start_receive();
        });
  }
public:
  net()
    : _io(),
    _socket(_io, asio::ip::udp::endpoint(asio::ip::udp::v4(), 2710)) {
    start_receive();
  }

  void poll() {
    _io.poll();
  }
};

