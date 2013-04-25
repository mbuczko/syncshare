var Syncshare = Syncshare || {};

Syncshare.Client = function(host) {
    return {
        lookup: function(service, options) {
            return new Syncshare.Service(host, service, options || {});
        }
    };
};

Syncshare.Service = function(host, service, options) {
    this.host = host;
    this.service = service;
    this.timeout = options.timeout || 60000;
    this.iframe = document.createElement('iframe');
    this.iframe.width = this.iframe.height = '0';

    var self = this;

    window.addEventListener('message', function(reply) {
        var data = reply.data, 
            type = data.type, 
            payload = data.payload
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
    this.iframe.src = 'http://' + this.host + '/syncshare/sse/' + this.service + '/frame?timeout='+this.timeout;
    
    document.body.appendChild(this.iframe);
    return this;
};
