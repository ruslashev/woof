-define(PROTOCOL_VERSION, 1).

-define(MESSAGE_TYPE_SPECIAL, 0).
-define(MESSAGE_TYPE_ISALIVE, 1).
-define(MESSAGE_TYPE_CONNECTION_REQ, 2).

-define(SERVER_MESSAGE_TYPE_SPECIAL, 0).
-define(SERVER_MESSAGE_TYPE_CONNECTION_REPLY, 1).
-define(SERVER_MESSAGE_TYPE_ERROR, 2).

-define(ERROR_TYPE_NOT_MATCHING_PROTOCOL, 0).

-record(packet,
        { reliable = undefined
        , sequence = 0
        , ack = 0
        , client_id = 0
        , num_messages = 0
        , serialized_messages = undefined
        }).

-record(client, { client_id, ip }).

-record(client_data,
        { client_key
        , unrel_messages = queue:new()
        , messages = queue:new()
        , unacked_packet_exists = false
        , unacked_packet = #packet{}
        , outgoing_sequence = 0
        , last_sequence_received = 0
        , sent_packets = 0
        , ack_packets = 0
        , received_packets = 0
        }).

