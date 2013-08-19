var Syncshare = (function() {

    var handlers = [];

    function connect(url, callback) {
        var socket = new easyXDM.Rpc({remote: url}, {
            local: {
                reply: function(json, successFn, errorFn) {
                    if (json && json.call && handlers[json.call]) {
                        handlers[json.call](json.data, json.type === 'broadcast');
                    }
                }
            },
            remote: {
                call: function() {}
            }
        });

        var session = {
            on: function(fn, callback) {
                handlers[fn] = callback;
                return this;
            },
            call: function(fn, params) {
                if (socket) {
                    socket.call(JSON.stringify({call: fn, data: (params || {})}));
                }
            }
        };

        if (callback && socket && session) {
            callback(session);
        } else {
            throw "Syncshare: could not establish connection.";
        }

        return session;
    };


    return {
        connect: connect
    };

}());
