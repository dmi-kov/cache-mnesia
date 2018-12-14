cache
=====

An OTP application to store cache via Mnesia 

Build and start app
-----

    $ make
    $ erl -pa ebin
    $ application:start(cache).


Usage api
-----

    $ cache:set(10, [{<<"name">>, <<"Jane">>}]).
    > ok

    $ cache:get(10).
    > [{<<"name">>, <<"Jane">>}]
    
    $ cache:get(20).
    > {error, not_found}
    
    $ cache:set(10, [{<<"name">>, <<"Jane">>}], [{ttl, 120}]).
    > ok
    
    $ cache:set(10, [{<<"name">>, <<"Jane">>}], [{ttl, infinity}]).
    > ok
