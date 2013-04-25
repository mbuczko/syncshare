{application,syncshare,
             [{description,"Syncshare server."},
              {vsn,"1"},
              {modules,[ini_handler,message_handler,rpc_handler,sse_handler,
                        syncshare,syncshare_amqp,syncshare_app,syncshare_sup]},
              {registered,[]},
              {applications,[kernel,stdlib,cowboy]},
              {mod,{syncshare_app,[]}},
              {env,[]}]}.
