var Syncshare = Syncshare || {};

Syncshare.Client = function(host, options) {
    return {
        connect: function(service) {
            return new Syncshare.Service(host, service, options || {});
        }
    };
};

Syncshare.Service = function(host, service, options) {
    var self = this, obj;

    this.host = host;
    this.service = service;
    this.sse = options.sse || false;
    this.timeout = options.timeout || 60000;

    if (options.sse) {

        // server side events initialization phase

        console.info('initializating SSE frame');

        window.addEventListener('message', function(reply) {
            var data = reply.data, 
                type = data.type, 
                payload = data.payload,
                handler = self.handlers[type];

            if (payload && handler) {
                handler.call(this, payload);
            }
        }, false);

        obj = this.channel = document.createElement('iframe');
        obj.width = obj.height = '0';
        obj.src = 'http://' + this.host + '/syncshare/sse/' + this.service + '/frame?token='+(options.token || "")+'&timeout='+this.timeout;

        document.body.appendChild(obj);

    } else {

        // websocket initialization phase

        console.info('initializating websockets');

        obj = this.channel = new WebSocket("ws://"+this.host + '/syncshare/wbs/' + this.service);
        obj.onopen = function() {
            console.log('Connected');
        };
        obj.onmessage = function(evt) {
            var data = evt.data.split('|'),
                type = data[0],
                payload = data[1],
                handler = self.handlers[type];

            console.log("Received: " + data);

            if (payload && handler) {
                handler.call(this, JSON.parse(payload));
            }
        };
        obj.onclose = function() {
            console.log('Connection closed');
        };
    }
};

Syncshare.Service.prototype.on = function(handlers) {
    this.handlers = handlers || {};
    return this;
};

Syncshare.Service.prototype.send = function(call, payload) {
    if (this.channel.contentWindow) {
        this.channel.contentWindow.postMessage({call: call, payload: payload}, '*');
    } else {
        this.channel.send(call + '|' + JSON.stringify(payload));
    }
    return this;
};

