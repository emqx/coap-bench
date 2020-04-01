-module(sim_worker).
-behaviour(gen_statem).

-export([start_link/0]).

-export([ terminate/3
        , code_change/4
        , init/1
        , callback_mode/0
        ]).

-export([ connect/3
        , candidate/3
        , leader/3
        ]).

start_link() ->
    gen_statem:start_link({local,?MODULE}, ?MODULE, [], []).

%% Mandatory callback functions
terminate(_Reason, _State, _Data) ->
    void.
code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

init([]) ->
    case earthwo_cluster:nodes() of
        [] -> %% I'm the first node in the cluster, so I'm going to be the leader!
            {next_state, leader, #{}, [heartbeat_timeout_action()]};
        _Nodes ->
            {ok, follower, #{}, [promote_timeout_action()]}
    end.

callback_mode() -> [state_functions].

%%% state callback(s)

follower(state_timeout, promote, Data) ->
    logger:info("try to promote to leader since the leader ~p not responding", [earthwo_cluster:get_leader(leader)]),
    earthwo_cluster:reset_leader(),
    AllNodes = earthwo_cluster:nodes(),
    MsgId = brodcast_msg(AllNodes, please_vote_me),
    {next_state, candidate,
        Data#{vote_id => MsgId, vote_rcvd => 1, not_vote_nodes => AllNodes, min_votes_n => (length(AllNodes) div 2)},
        [{state_timeout, ?T_ELECT_RESP, no_sufficent_votes}]};
follower(info, {please_vote_me, FromNode, VoteId}, _Data) ->
    case earthwo_cluster:get_leader(leader) of
        undefined ->
            logger:info("got vote request ~p from ~p, vote it", [VoteId, FromNode]),
            send_msg(vote, FromNode, VoteId),
            {keep_state_and_data, [promote_timeout_action()]};
        LeaderNode ->
            logger:warning("ignore the vote request ~p from ~p as I have a leader: ~p", [VoteId, FromNode, LeaderNode]),
            keep_state_and_data
    end;
follower(EventType, EventContent, Data) ->
    ?handle_events(EventType, EventContent, Data).

candidate(state_timeout, no_sufficent_votes, Data) ->
    logger:warning("no sufficent votes in ~p ms, demotes to follower. Vote state: ~p", [?T_ELECT_RESP, Data]),
    {next_state, follower, #{}, [promote_timeout_action()]};
candidate(info, {please_vote_me, FromNode, VoteId}, _Data) ->
    logger:warning("ignore the vote request ~p from ~p as I want to be the leader too", [VoteId, FromNode]),
    keep_state_and_data;
candidate(info, {vote, FromNode, VoteId}, Data = #{vote_id := VoteId, not_vote_nodes := AllNodes, vote_rcvd := VoteRcvd, min_votes_n := MinVotesN}) ->
    case AllNodes -- [FromNode] of
        AllNodes ->
            logger:warning("ignore vote from unknow node: ~p, vote_id: ~p", [FromNode, VoteId]),
            keep_state_and_data;
        RemNodes ->
            case VoteRcvd + 1 of
                VoteRcvdNow when VoteRcvdNow > MinVotesN ->
                    {next_state, leader, #{}, [heartbeat_timeout_action()]};
                VoteRcvdNow ->
                    {keep_state, Data#{vote_rcvd => VoteRcvdNow, not_vote_nodes => RemNodes}}
            end
    end;
candidate(info, {vote, FromNode, VoteId}, _Data) ->
    logger:warning("ignore outdated vote from node: ~p, vote_id: ~p", [FromNode, VoteId]),
    keep_state_and_data;
candidate(EventType, EventContent, Data) ->
    ?handle_events(EventType, EventContent, Data).

leader(state_timeout, heartbeat, Data = #{not_sync_counters := RemNodes}) ->
    AllNodes = earthwo_cluster:nodes(),
    SyncId = brodcast_msg(AllNodes, {sync_topology, earthwo_cluster:local_topology()}),
    PendingCounters =
         remove_timeout_nodes(fun(_, N) -> N >= 2 end, incr_node_counters(RemNodes)),
    NodeCounters = maps:merge(init_node_counters(AllNodes), PendingCounters),
    {keep_state, Data#{sync_topology_id => SyncId, not_sync_counters => NodeCounters}, [heartbeat_timeout_action()]};
leader(state_timeout, heartbeat, Data) ->
    AllNodes = earthwo_cluster:nodes(),
    SyncId = brodcast_msg(AllNodes, {sync_topology, earthwo_cluster:local_topology()}),
    {keep_state, Data#{sync_topology_id => SyncId, not_sync_counters => init_node_counters(AllNodes)}, [heartbeat_timeout_action()]};
leader(info, {sync_topology_deny, FromNode, SyncId}, Data = #{sync_topology_id := SyncId, not_sync_counters := RemNodes}) ->
    ok = earthwo_cluster:remove_node(FromNode),
    {keep_state, Data#{not_sync_counters => maps:remove(FromNode, RemNodes)}};
leader(info, {sync_topology_ok, FromNode, SyncId}, Data = #{sync_topology_id := SyncId, not_sync_counters := RemNodes}) ->
    {keep_state, Data#{not_sync_counters => maps:remove(FromNode, RemNodes)}};
leader(info, {Msg, FromNode, _MsgId}, _Data) ->
    logger:warning("ignore unexpected msg from ~p, msg: ~p", [FromNode, Msg]),
    keep_state_and_data;
leader(EventType, EventContent, Data) ->
    ?handle_events(EventType, EventContent, Data).

handle_common_events(StateName, info, {{sync_topology, Topology}, LeaderNode, SyncId}, Data) ->
    case maps:get(leader, Data, none) of
        LeaderNode ->
            logger:debug("received sync_topology from leader: ~p when in state: ~p, topology: ~p", [LeaderNode, StateName, Topology]);
        OldLeader ->
            logger:info("follow to new leader: ~p when in state: ~p, topology: ~p, oldleader: ~p", [LeaderNode, StateName, Topology, OldLeader]),
            ok = earthwo_cluster:set_leader(LeaderNode)
    end,
    case earthwo_cluster:apply_topology(Topology) of
        ok -> send_msg(sync_topology_ack, LeaderNode, SyncId);
        {error, not_consistent} -> send_msg(sync_topology_deny, LeaderNode, SyncId)
    end,
    {next_state, follower, #{leader => LeaderNode}, [promote_timeout_action()]};
handle_common_events(StateName, EventType, EventContent, _Data) ->
    logger:warning("received unexpected msg ~p when in state: ~p", [{EventType, EventContent}, StateName]),
    keep_state_and_data.

%% ========================================================

brodcast_msg(Nodes, Msg) ->
    MId = erlang:make_ref(),
    [send_msg(Msg, N, MId) || N <- Nodes],
    MId.

send_msg(Msg, Node, MId) ->
    logger:debug("send to node: ~p, mid: ~p, msg: ~p", [Node, MId, Msg]),
    erlang:send({?MODULE, Node}, {Msg, node(), MId}).

promote_timeout_action() ->
    {state_timeout, election_timout(?T_ELECT), promote}.

heartbeat_timeout_action() ->
    {state_timeout, ?T_HEATBEAT, heartbeat}.

election_timout(BaseT) ->
    BaseT + rand:uniform(BaseT).

init_node_counters(Nodes) ->
    lists:foldl(
        fun(Node, Counters) ->
            Counters#{Node => 0}
        end, #{}, Nodes).

incr_node_counters(Counters) ->
    maps:map(fun(_, N) -> N + 1 end, Counters).

remove_timeout_nodes(Pred, Counters) ->
    It = maps:iterator(Counters),
    do_fileter_node_counters(Pred, maps:next(It), Counters).

do_fileter_node_counters(_Pred, none, RemCounters) ->
    RemCounters;

do_fileter_node_counters(Pred, {Node, N, It}, RemCounters) ->
    case Pred(Node, N) of
        true ->
            ok = earthwo_cluster:remove_node(Node),
            do_fileter_node_counters(Pred, maps:next(It), maps:remove(Node, RemCounters));
        false ->
            do_fileter_node_counters(Pred, maps:next(It), RemCounters)
    end.
