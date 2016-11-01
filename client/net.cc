#include "net.hh"
#include "utils.hh"
#include <fstream>

using asio::ip::udp;

class server {
  asio::ip::udp::socket _socket;
  asio::ip::udp::endpoint _remote_endpoint;
  std::array<char, 128> _recv_buffer;
  void start_receive() {
    _socket.async_receive_from(asio::buffer(_recv_buffer), _remote_endpoint
        , [this](const asio::error_code &e, std::size_t bytes_rx) {
          if (e || bytes_rx == 0) {
            warning("something is wrong");
            start_receive();
          } else {
            puts("receive:");
            for (auto c : _recv_buffer)
              if (c)
                printf("%c", c);
              else
                break;
            puts("");
          }
        });
  }
public:
  server(asio::io_service& io_service)
    : _socket(io_service, udp::endpoint(udp::v4(), 2710))
  {
    start_receive();
  }
};

net::net() {
  asio::io_service io_service;
  server s(io_service);
  io_service.run();
}

