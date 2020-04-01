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
        /      \ (data_set_group)
    sim_group ... sim_group
    /   \        /   \  (work_flow)
sim ... sim   sim ... sim
```
