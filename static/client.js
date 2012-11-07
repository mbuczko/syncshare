var Syncshare = Syncshare || {};

Syncshare.Client = function(host) {
    window.addEventListener('message', function(message) {
        console.log(message.data.reply);
    }, false);

    return {
        lookup: function(service, options) {
            return new Syncshare.Service(host, service);
        }
    };
};


Syncshare.Service = function(host, service) {
    this.host = host;
    this.service = service;
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
    this.iframe.src = 'http://' + this.host + '/syncshare/' + this.service + '/init';
    
    document.body.appendChild(this.iframe);
    return this;
};
