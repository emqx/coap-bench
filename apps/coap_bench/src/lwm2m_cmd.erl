-module(lwm2m_cmd).

-export([ make_register/4
        , make_deregister/2
        , make_notify/3
        , ack_validator/2
        , location_path/1
        , make_ack/4
        , uri_path/1
        , token/1
        ]).

-include_lib("lwm2m_coap/include/coap.hrl").

make_register(EndpointName, Lifetime, MsgId, ObjectLinks) ->
    Lt = integer_to_binary(Lifetime),
    Query = [<<"b=U">>, <<"ep=", EndpointName/binary>>, <<"lt=", Lt/binary>>, <<"lwm2m=1.0">>],
    coap_message(con, post, ObjectLinks, MsgId, crypto:strong_rand_bytes(4),
        [{uri_path, [<<"rd">>]}, {uri_query,Query}, {content_format, <<"application/link-format">>}]).

make_deregister(Location, MsgId) when is_list(Location) ->
    coap_message(con, delete, <<>>, MsgId, crypto:strong_rand_bytes(4),
        [{uri_path, Location}]).

make_notify(Token, MsgId, Body) ->
    coap_message(non, {ok, content}, Body, MsgId, Token,
        [{observe, rand:uniform(9999)}, {content_format, <<"application/vnd.oma.lwm2m+tlv">>}]).

coap_message(Type, Method, Payload, MsgId, Token, Options) ->
    #coap_message{
        type = Type,
        method = Method,
        id = MsgId,
        token = Token,
        payload = Payload,
        options = Options
    }.

make_ack(#coap_message{id = MsgId, token = Token}, Code, Payload, Options) ->
    #coap_message{
        type = ack, id = MsgId,
        token = Token,
        method = Code,
        payload = Payload,
        options = Options
    }.

ack_validator(#coap_message{type = ack, id = MsgId}, MsgId) -> true;
ack_validator(_, _) -> false.

location_path(#coap_message{options = Options}) ->
    proplists:get_value(location_path, Options, []).

uri_path(#coap_message{options = Options}) ->
    proplists:get_value(uri_path, Options, []).

token(#coap_message{token = Token}) ->
    Token.
