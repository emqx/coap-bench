-module(lwm2m_cmd).

-export([ make_register/3
        ]).

-include_lib("lwm2m_coap/include/coap.hrl").

make_register(EndpointName, Lifetime, MsgId) ->
    Lt = integer_to_binary(Lifetime),
    Query = [<<"ep=", EndpointName/binary>>, <<"lt=", Lt/binary>>],
    coap_message(con, get, <<>>, MsgId,
        [{uri_path, [<<"rd">>]}, {uri_query,Query}]).

coap_message(Type, Method, Payload, MsgId, Options) ->
    Request = #coap_message{
                type = Type,
                method = Method,
                id = MsgId,
                token = crypto:strong_rand_bytes(4),
                payload = Payload,
                options = Options
             },
    lwm2m_coap_message_parser:encode(Request).

