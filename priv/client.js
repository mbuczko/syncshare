var Syncshare = Syncshare || {};

Syncshare.Client = function(host) {
    this.host = host;
};

Syncshare.Client.prototype.lookup = function(service) {
    var iframe = document.createElement("iframe");
    iframe.src = "http://" + this.host + "/init?service="+service;
    iframe.width = iframe.height = '0';
    document.body.appendChild(iframe);

    window.addEventListener("message", function(message) {
        console.log(message.data.reply);
    }, false);

    this.service = service;
};
