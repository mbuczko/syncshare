console.log('loaded twitter.js provider');


Syncshare = (function(service, proto, auth) {

    function connect(callback) {
        var channel, socket;

        if ("WebSocket" in window && (proto === 'auto' || proto === 'websockets')) {

            console.log('going with WEBSOCKETS', service);

            channel = new WebSocket("ws://" + 'localhost:8080/syncshare/wbs/' + service);
            channel.onopen = function() {
                console.log('Connected');
                socket = callback ? callback(this) : null;
            };
            channel.onmessage = function(evt) {
                if (socket && evt.data) {
                    var data = evt.data.split("|", 2);
                    socket.reply({call: data[0], type: data[1], data: JSON.parse(evt.data.substring(data[0].length+data[1].length+2))});
                }
            };
            channel.onclose = function() {
                console.log('Connection closed');
            };
        } else {
            console.log('going with SSE', service);
            
            channel = new EventSource('/syncshare/sse/' + service);
            // channel.addEventListener('message', function(reply) { rpc.postMessage({type: 'message', payload: reply.data}); });
            // channel.addEventListener('broadcast', function(reply) { rpc.postMessage({type: 'broadcast', payload: reply.data}); });
        }        
    }

    function send(channel, fn, data) {
        if (channel.send) {
            channel.send(fn + "|" + auth + "|" + JSON.stringify(data || ""));
        }
    }

    connect(function(channel) {
        return new easyXDM.Rpc({},{ 
            local: { 
                call: function(message, successFn, errorFn) {
                    var json = JSON.parse(message);
                    if (json.call) {
                        send(channel, json.call, json.data);
                    }
                }
            },
            remote: {
                reply: function() { }

            }
        });
    });

    return {
        connect: connect
    };

})('twitter', 'auto', '123');
