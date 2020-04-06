coap_bench
==========

This tool runs as a local services and send msgs for benchmarking your CoAP or LwM2M server. Each LwM2M simulator will run multiple tasks sequentially specified in the workflow file.

Build
-----

    $ make rel

Usage
-----

    $ cd _build/default/rel/coap_bench

    $ ./bin/coap_bench start

    $ ./bin/coap_bench load data/client_info.csv data/workflow.json

    Loading profiles into memory...

    9 clients loaded from client info file:	"data/client_info.csv"
    2 groups  loaded from workflow file:	"data/workflow.json"

    $ ./bin/coap_bench run -F -H 127.0.0.1 -P 5683 -B "127.0.0.1"
    Force start test with conf: #{binds => [{127,0,0,1}],
                              conn_interval => 10,
                              host => {127,0,0,1},
                              port => 5683}

    $ ./bin/coap_bench status
    ============================================================
    RunningTaskGroup                                RunningSims
    ------------------------------------------------------------
    sim_group_notify                                : 0
    sim_group_register_only                         : 0

    TaskGroup Total: 2, Sims Total: 0
    ============================================================
    ============================================================
    Metrics                                         Value
    ------------------------------------------------------------
    REGISTER_SUCC                                   : 9
    REGISTER_FAIL                                   : 0
    REGISTER                                        : 9
    NOTIFY                                          : 4
    DEREGISTER_SUCC                                 : 9
    DEREGISTER_FAIL                                 : 0
    DEREGISTER                                      : 9
    WAIT_OBSERVE                                    : 8
    WAIT_OBSERVE_SUCC                               : 8
    WAIT_OBSERVE_FAIL                               : 0
    CON_SENT                                        : 18
    CON_SEND_FAIL                                   : 0
    CON_RCVD                                        : 8
    ACK_SENT                                        : 8
    ACK_SEND_FAIL                                   : 0
    ACK_RCVD                                        : 18
    NON_SENT                                        : 4
    NON_SEND_FAIL                                   : 0
    NON_RCVD                                        : 0
    RST_SENT                                        : 0
    RST_SEND_FAIL                                   : 0
    RST_RCVD                                        : 0
    ============================================================

    $ ./bin/coap_bench clear
    Stop all task groups

    $ ./bin/coap_bench stop
    ok

Design
------

```
        sim_manager
        /        \
    sim_group ... sim_group
    /   \         /    \
sim ... sim     sim ... sim
```

Client Info file
----------------

Should be of csv type:

An example client info file with only the endpoint name:

```
874625413356234
874625413356235
```

Another example client info file that contains epname, lifetime, and PSK:

```
874625413356234,60,beifnigbabef
874625413356235,120,exnaoeitbqid
```

Workflow
--------

An example workflow for LwM2M:

[
    {
        "group_name":"register_only",
        "weight": 1,
        "work_flow":[
            {
                "task":"register",
                "ep": "$1",
                "lifetime": "$2"
            },
            {
                "task":"sleep",
                "interval":120
            },
            {
                "task":"deregister"
            }
        ]
    },
    {
        "group_name":"notify",
        "weight": 1
        "work_flow":[
            {
                "task":"register",
                "lifetime":60
            },
            {
                "task":"wait_observe",
                "path":"19/0/0",
                "timeout":120
            },
            {
                "task":"notify",
                "path":"19/0/0",
                "body": {"type": "auto_gen_binary", "size": 12}
            },
            {
                "task":"deregister"
            }
        ]
    }
]

TODO: Support task repeat

"work_flow":[
    {
        "task":"register",
        "lifetime":60
    },
    {
        "task": "repeat",
        "repeat_num": 3,
        "flow": [
            {
                "task":"sleep",
                "interval":120
            },
            {
                "task":"notify",
                "body": {"type": "auto_gen_binary", "size": 12}
            }
        ]
    },
    {
        "task":"deregister"
    }
]

- Support variables in the workflow, e.g. "$1" denotes the first field of the current line in the client info file, and "$2" denotes the second one, and so on.
