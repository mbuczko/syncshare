console.log('loaded twitter.js provider');


Syncshare = (function(service, proto, auth) {

    var socket;

    function connect(callback) {
        var channel;

        if ("WebSocket" in window) {

            console.log('going with WEBSOCKETS', service);

            channel = new WebSocket("ws://" + 'localhost:8080/syncshare/wbs/' + service);
            channel.onopen = function() {
                console.log('Connected');
                if (callback) { callback(channel); }
            };
            channel.onmessage = function(evt) {
                if (socket && evt.data) {
                    var data = evt.data.split("|", 2);
                    socket.postMessage(JSON.stringify({call: data[0], type: data[1], data: JSON.parse(evt.data.substring(data[0].length+data[1].length+2))}));
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
            channel.send(fn + "|" + auth + "|" + (data || ""));
        }
    }

    connect(function(channel) {
        socket = new easyXDM.Socket({
            onMessage: function(message, origin) {
                var json = JSON.parse(message);

                console.log('MESSAGE', json);

                if (json.call) {
                    send(channel, json.call, json.data);
                }                    
            }
        });
    });

})('twitter', 'auto', '123');


