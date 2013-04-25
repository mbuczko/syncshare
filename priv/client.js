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
        var data = reply.data, direct = data.direct, broadcast = data.broadcast, handlers = self.handlers;
        if (direct && handlers.direct) {
            handlers.direct.call(this, direct);
        } else
        if (broadcast && handlers.broadcast) {
            handlers.broadcast.call(this, broadcast);
        }
    }, false);
};

Syncshare.Service.prototype.on = function(handlers) {
    this.handlers = handlers || {};
    return this;
};

Syncshare.Service.prototype.direct = function(call, params) {
    this.iframe.contentWindow.postMessage({service: this.service, call: call, params: JSON.stringify(params) }, '*');
    return this;
};

Syncshare.Service.prototype.start = function() {
    this.iframe.src = 'http://' + this.host + '/syncshare/' + this.service + '/init?timeout='+this.timeout;
    
    document.body.appendChild(this.iframe);
    return this;
};
