-define(PROTOCOL_VERSION, 1).

-define(MESSAGE_TYPE_SPECIAL, 0).
-define(MESSAGE_TYPE_PING, 1).
-define(MESSAGE_TYPE_CONNECTION_REQ, 2).

-define(SERVER_MESSAGE_TYPE_SPECIAL, 0).
-define(SERVER_MESSAGE_TYPE_ERROR, 1).
-define(SERVER_MESSAGE_TYPE_PONG, 2).
-define(SERVER_MESSAGE_TYPE_CONNECTION_REPLY, 3).
-define(SERVER_MESSAGE_TYPE_UPDATE, 4).

-define(ERROR_TYPE_NOT_MATCHING_PROTOCOL, 0).

-define(SERVER_UPDATE_SEND_DELAY_MS, 500).
-define(CLIENT_TIMEOUT_MS, 5000).

-record(packet,
        { reliable = undefined
        , sequence = 0
        , ack = 0
        , client_id = 0
        , num_messages = 0
        , serialized_messages = undefined
        }).

-record(client_data,
        { client_id
        , remote_ip
        , unrel_messages = queue:new()
        , messages = queue:new()
        , unacked_packet_exists = false
        , unacked_packet = #packet{}
        , outgoing_sequence = 0
        , last_sequence_received = 0
        , sent_packets = 0
        , ack_packets = 0
        , received_packets = 0
        , time_since_last_ping = 0
        , position_x = 0
        , position_y = 0
        , alive = true
        , color = woof_utils:generate_random_color()
        }).

