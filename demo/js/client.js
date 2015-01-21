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
    var self = this, handlers = {}, frame, rpc, promise = new Syncshare.Deferred();

    this.host = host;
	this.transport = options.transport || 'wbs';
    this.timeout = options.timeout || 60000;
	this.session = {
        on: function(fn, callback) {
			if (!handlers[fn]) {
				handlers[fn] = callback;
			}
            return this;
        },
        call: function(fn, params, deferred) {
            var dfid = deferred ? deferred.id() : null;

			if (fn) {
				console.log('CALL', dfid ? 'RPC '+fn+' (id='+dfid+')' : fn);

				if (dfid != null) {
					handlers[dfid] = deferred;
				}
				frame.contentWindow.postMessage({call: fn, data: params, deferred: dfid}, '*');
				return this;				
			}

			this.callFn = null;
			return false;
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

			// deferred RPC function?
			if (dfid) {
				if (data.success) {
					if (call === '_connect') {
						console.log('RPC initialized', data.data);

						session.remotes = data.data;

						// do not resolve deferred object if authentication was forced
						// 'authenticate' callback should take care of this

						if (!options.auth) {
							promise.resolve(session);
						}

					} 
					else { handlers[dfid].resolve(data.data); }
				} 
				else { handlers[dfid].reject(data.data); }
				delete handlers[dfid];
			} 
			else { handlers[call](data.data, data.type === 'broadcast'); }
        }
    }, false);

    frame = document.createElement('iframe');
    frame.width = frame.height = '0';
    frame.src = this.host + '?timeout='+this.timeout+'&transport='+this.transport;

    document.body.appendChild(frame);
	
	frame.onload = function() {
		session.on('authenticate', function(auth) {
			console.log('Authenticated', auth);

			if (auth || auth === undefined) {

				console.log('Session authenticated', this.authenticated);

				// session could be already authenticated. in this case it's better not 
				// to resolve promise as the promise success-handler may add some bindings again

				if (!this.authenticated) {
					this.authenticated = true;
					promise.resolve(session);
				}
			} else {
				this.authenticated = false;
				console.warn('Authentication failed or session expired.', auth);
			}
		}).call('_connect', null, promise);
	};
    return promise;
};

Syncshare.Rpc = function(session) {
	for (var deferred,call,i=0,n=session.remotes.length; i<n; i++) {
		call = session.remotes[i];

		this[call] = function(fn) {
			return function() {
				deferred = new Syncshare.Deferred();
				session.call(fn, Array.prototype.slice.call(arguments), deferred);
				return deferred;
			};
		}(call);
	};
};

Syncshare.Deferred = function() {
    var dfid = new Date().getTime()+Math.floor((Math.random()*1000)),
	    result, callbacks = {
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
