#include "net.hh"
#include "utils.hh"

connection::connection() {
  assertf(enet_initialize() == 0, "failed to init enet");

  _client = enet_host_create(nullptr, 1/* connection */, 2/* channels */, 0, 0);
  assertf(_client != nullptr, "failed to create enet client host");

  enet_address_set_host (&_address, "127.0.0.1");
  _address.port = port;
  _peer = enet_host_connect(_client, &_address, 2, 0);
  assertf(_peer != nullptr, "no available peers for initializing an enet "
      "connection");

  ENetEvent event;
  if (enet_host_service(_client, &event, 4000) > 0 &&
      event.type == ENET_EVENT_TYPE_CONNECT) {
    puts("connection succeeded");
  } else {
    enet_peer_reset(_peer);
    puts("connection failed");
  }
}

connection::~connection() {
  enet_host_destroy(_client);
  enet_deinitialize();
}

void connection::poll() {
  ENetEvent event;
  while (enet_host_service(_client, &event, 0) > 0) {
    switch (event.type) {
      case ENET_EVENT_TYPE_NONE:
        break;
      case ENET_EVENT_TYPE_CONNECT:
        printf("A new client connected from %x:%u.\n",
            event.peer->address.host,
            event.peer->address.port);
        break;
      case ENET_EVENT_TYPE_RECEIVE:
        printf("A packet of length %u containing %s was received from %s on "
            "channel %u.\n",
            event.packet->dataLength,
            event.packet->data,
            event.peer->data,
            event.channelID);
        enet_packet_destroy (event.packet);
        break;
      case ENET_EVENT_TYPE_DISCONNECT:
        printf ("client disconnected.\n");
    }
  }
}

