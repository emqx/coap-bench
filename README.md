coap_bench
=====

An OTP application

Build
-----

    $ rebar3 compile

Usage
-----

    $ COAP_NODE_NAME='coap1@127.0.0.1' ./bin/coap_bench console

Design
------

```
                        run -----
                                |
                                V
    NODE1                     NODE2                       NODE3
                (data_set1)              (data_set2)
sim_manager  <----------  sim_manager -------------->  sim_manager
                        (calc_data_sets)


        sim_manager
        /        \             (data_set_group)
    sim_group ... sim_group
    /   \         /    \       (work_flow)
sim ... sim     sim ... sim
```

Data file
---------

Should be of csv type:

```
beifnigbabef,60
exnifeitbqid,120
...
```

Task distribution
-----------------

Auto distribute by NIC numbers of each node:

```
{
    "distribute": "auto",
    "nodes": [
        "coap_bench1@192.168.1.21",
        "coap_bench2@192.168.1.22"
    ]
}
```

Or set the weight of each node manually:

```
{
    "distribute": "weight",
    "tasks": {
        "coap_bench1@192.168.1.21": 10, %% we suggest set weight == (num of NICs of current node)
        "coap_bench2@192.168.1.22": 10
    }
}
```

It would be nice if we could set the exact sim numbers on each node:

```
{
    "distribute": "count",
    "tasks": {
        "coap_bench1@192.168.1.21": 30000,
        "coap_bench2@192.168.1.22": 50000
    }
}
```

Or by range?

```
{
    "distribute": "range",
    "tasks": {
        "coap_bench1@192.168.1.21": 0-29999,
        "coap_bench2@192.168.1.22": 30000-49999
    }
}
```

Workflow
--------

[
    {
        "group_name":"register_only",
        "type": "lwm2m",
        "weight": 1,
        "work_flow":[
            {
                "cmd":"register",
                "ep": "$1",
                "lifetime": "$2"
            },
            {
                "cmd":"sleep",
                "interval":120
            },
            {
                "cmd":"deregister"
            }
        ]
    },
    {
        "group_name":"notify",
        "weight": 1
        "work_flow":[
            {
                "cmd":"register",
                "lifetime":60
            },
            {
                "cmd":"sleep",
                "interval":120
            },
            {
                "cmd":"notify",
                "body": {"type": "auto_gen_binary", "size": 12}
            },
            {
                "cmd":"deregister"
            }
        ]
    }
]

- Support variables in the workflow, e.g. "$1" denotes the first field of the current line in the data file, and "$2" denotes the second one, and so on.

- 