var Syncshare = Syncshare || {};

Syncshare.Client = function(host, options) {
    return {
        lookup: function(service) {
            return new Syncshare.Service(host, service, options || {});
        }
    };
};

Syncshare.Service = function(host, service, options) {
    this.host = host;
    this.service = service;
    this.sse = options.sse || false;
    this.timeout = options.timeout || 60000;
    this.iframe = document.createElement('iframe');
    this.iframe.width = this.iframe.height = '0';

    var self = this;

    window.addEventListener('message', function(reply) {
        var data = reply.data, 
            type = data.type, 
            payload = data.payload,
            handler = self.handlers[type];

        if (payload && handler) {
            handler.call(this, payload);
        }
    }, false);
};

Syncshare.Service.prototype.on = function(handlers) {
    this.handlers = handlers || {};
    return this;
};

Syncshare.Service.prototype.send = function(call, params) {
    this.iframe.contentWindow.postMessage({service: this.service, call: call, params: JSON.stringify(params) }, '*');
    return this;
};

Syncshare.Service.prototype.start = function() {

    // should we use SSE? 
    // if so, let's fetch the frame which will communicate with server (to overcome ajax restrictions)

    if (this.sse) {
        this.iframe.src = 'http://' + this.host + '/syncshare/sse/' + this.service + '/frame?timeout='+this.timeout;
        document.body.appendChild(this.iframe);
    } else {


        // don't want SSE? let's switch to websockets instead.
        // websocket are turned on by default 

        var ws = new WebSocket("ws://"+this.host + '/syncshare/wbs/' + this.service), self = this;

        ws.onopen = function() {
            console.log('Connected');
        };
        ws.onmessage = function(evt) {
            var data = evt.data,
                type = data.type,
                payload = data.payload,
                handler = self.handlers[type];

            console.log("Received: " + data);

            if (payload && handler) {
                handler.call(this, payload);
            }
        };
        ws.onclose = function() {
            console.log('Connection closed');
        };
    }
    return this;
};
