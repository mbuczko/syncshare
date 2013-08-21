var Syncshare = Syncshare || {};

Syncshare.Client = function(host, options) {
    return {
        connect: function(callback) {
			new Syncshare.Service(host, options || {}).then(callback);
            return this;
        }
    };
};

Syncshare.Service = function(host, options) {
    var self = this, handlers = {}, frame, rpc, deferred = new Syncshare.Deferred();

    this.host = host;
    this.timeout = options.timeout || 60000;
    this.token = options.token || "";
	this.session = {
        on: function(fn, callback) {
            handlers[fn] = callback;
            return this;
        },
        call: function(fn, params, deferred) {
            var dfid = deferred ? deferred.id() : null;

            if (dfid != null) {
                handlers[dfid] = deferred;
            }
            frame.contentWindow.postMessage({call: fn, data: params, deferred: dfid}, '*');
            return this;
        },
        rpc: function(callback) {
            rpc = rpc || new Syncshare.Rpc(this);
            return rpc;
        }
	};

	var session = this.session;

    window.addEventListener('message', function(response) {
        var data = response.data,
			call = data.call,
            dfid = data.deferred;

        if (call && handlers[dfid || call]) {
			if (dfid) {
				if (data.success) {
					// connection established. expose session.
					if (call === '_connect') {
						console.log('Connection established => remotes', data.data);

						session.remotes = data.data;
						deferred.resolve(session);
					} else {
						handlers[dfid].resolve(data.data);
					}
				} else {
					handlers[dfid].reject(data.data);
				}
				delete handlers[dfid];
			} else {
				handlers[call](data.data, data.type === 'broadcast');
			}
        }
    }, false);

    frame = document.createElement('iframe');
    frame.width = frame.height = '0';
    frame.src = this.host + '?token='+this.token+'&timeout='+this.timeout;

    document.body.appendChild(frame);
	
	frame.onload = function() {
		session.call('_connect', null, deferred);
	};
    return deferred;
};

Syncshare.Rpc = function(service) {
	for (var deferred,len,fn,i=0,n=service.remotes.length; i<n; i++) {        
		fn = service.remotes[i];        
		this[fn] = function() {
			deferred = new Syncshare.Deferred();
			service.call(fn, Array.prototype.slice.call(arguments), deferred);
			return deferred;
		};
	};
};

Syncshare.Deferred = function() {
    var dfid, result, callbacks = {
        resolved : [],
        rejected : []
    };

    function done(solution, res) {
        var fns = callbacks[solution];

        for (var i=0, n=fns.length; i<n; i++) {
            fns[i].call(null, res);
        }

        result = res;
        return true;
    };

	// generate unique identifier
	dfid = new Date().getTime();

    return {
        id: function() { return dfid; },
        then: function(successFn, errorFn, context) {
            if (this.isResolved) {
                successFn.call(context, result);
            } else
            if (this.isRejected) {
                errorFn.call(context, result);
            } else {
                if (successFn) {
                    callbacks.resolved.push(successFn);                    
                }
                if (errorFn) {
                    callbacks.rejected.push(errorFn);                    
                }
            }
            return this;
        },
        resolve: function(arg) {
            this.isResolved = done('resolved', arg);
        },
        reject: function(arg) {
            this.isRejected = done('rejected', arg);
        }
    };
};
