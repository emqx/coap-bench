-module(sim_trash).

-export([ start_link/0
        , take_socket/1
        ]).

-export([trash_loop/1]).

start_link() ->
    Pid = proc_lib:spawn_link(?MODULE, trash_loop, [#{}]),
    true = register(?MODULE, Pid),
    {ok, Pid}.

trash_loop(State) ->
    receive
        {udp, Sock, PeerIP, PeerPortNo, Packet} ->
            try
                lwm2m_coap_message_parser:decode(Packet)
            of
                CoapMsg ->
                    coap_bench_message:incr_counter_rcvd(CoapMsg),
                    logger:error("[sim_trash] received coap message: ~p, sockname: ~p", [CoapMsg, inet:sockname(Sock)]),
                    sim_worker:may_ack_it(Sock, PeerIP, PeerPortNo, CoapMsg)
            catch
                _:_ ->
                    logger:error("[sim_trash] received unknown udp message: ~p", [Packet])
            end,
            trash_loop(State);
        Msg ->
            logger:error("[sim_trash] received unexpected message: ~p", [Msg]),
            trash_loop(State)
    end.

take_socket(Sock) ->
    gen_udp:controlling_process(Sock, erlang:whereis(?MODULE)).
