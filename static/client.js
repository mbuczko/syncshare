var Syncshare = Syncshare || {};

Syncshare.Client = function(host) {
    window.addEventListener('message', function(reply) {
        if (reply.data.rpc) {
            console.log('syncshare: got rpc response: ', reply.data.rpc);
        } else {
            console.log('syncshare: got public push: ', reply.data.broadcast);
        }
    }, false);

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

};

Syncshare.Service.prototype.on = function(events) {
    return this;
};

Syncshare.Service.prototype.rpc = function(call, params) {
    this.iframe.contentWindow.postMessage({service: this.service, call: call, params: JSON.stringify(params) }, '*');
    return this;
};

Syncshare.Service.prototype.start = function() {
    this.iframe.src = 'http://' + this.host + '/syncshare/' + this.service + '/init?timeout='+this.timeout;
    
    document.body.appendChild(this.iframe);
    return this;
};
