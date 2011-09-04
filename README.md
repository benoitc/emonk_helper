emonk_helper
============

Some [emonk](http://github.com/davisp/emonk) extensions to ease its usage
in applications.

For now it provides an emonk contexts pool management. Each emonk
context is a thread so you have to reduce their number. 

The pool allows simple javascript call in a context and multiple  javascriptcall to a context.


Requirements
------------

1. Latest emonk.
2. R14A or superior.

Build
-----


    $ git clone git://github.com/benoitc/emonk_helper.git
    $ cd emonk_helper
    $ make

Run
---

    1> application:start(emonk).
    ok
    2> application:start(emonk_helper).
    ok
    3> emonk_helper:start_pool(js_pool, 10).
    {ok,<0.41.0>}
    4> emonk_ctx_pool:poolsize(js_pool).
    10
    5> emonk_helper:eval(js_pool, <<"var f = 2; f*3;">>, 3).
    {ok,6}
    6> emonk_ctx_pool:poolsize(js_pool).                    
    10
    7> {ok, Ctx} = emonk_helper:reserve_batch_ctx(js_pool, 4).
    {ok,<0.47.0>}
    8> emonk_ctx_pool:poolsize(js_pool).                      
    9
    10> emonk_ctx:eval(Ctx, <<"var g = function(x) {return x*4;};">>).
    {ok,undefined}
    11> emonk_ctx:call(Ctx, <<"g">>, [9]).                            
    {ok,36}
    12> emonk_ctx_pool:poolsize(js_pool).                            
    9
    13> emonk_ctx:finish_batch(Ctx).     
    ok
    14> emonk_ctx_pool:poolsize(js_pool).
    10


