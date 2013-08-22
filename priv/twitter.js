console.log('loaded twitter.js provider');

Syncshare = (function(service, proto) {
    var channel, remotes = {};

    var request = function(url) {
        var xhr, encodeQuery = function(data) {
			var ret = [];
			for (var d in data) { 
				ret.push(encodeURIComponent(d) + '=' + encodeURIComponent(data[d])); 
			}
			return ret.join('&');
		};
        try { 
            xhr = new XMLHttpRequest();
        }
        catch (e) {
            try { 
                xhr = new ActiveXObject('Microsoft.XMLHTTP'); 
            }
            catch (e) {
                xhr = new ActiveXObject('Msxml2.XMLHTTP'); 
            }
        }
        this.post = function(payload) {
            xhr.open('POST', url, true);
            xhr.setRequestHeader('Method', 'POST '+url+' HTTP/1.1');
            xhr.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');
            xhr.send('payload='+JSON.stringify(payload));
        };
		this.get = function(payload) {
			if (payload) {
				url += '?'+encodeQuery(payload);
			}
		    xhr.open('GET', url, true);
			xhr.send();
		};
    };

    var remote = function(fns) { remotes = fns; },
		reply  = function(call, type, data, dfid, succ) { 
			window.parent.postMessage({
				call : call, 
				type : type, 
				data : data, 
				deferred : dfid, 
				success  : succ}, '*'); 
		};

	window.addEventListener('message', function(e) {
		var dfid = e.data.deferred,
			data = e.data.data, 
			fn = e.data.call;

		// remote call
		// trying to invoke function directly

		if (dfid) {
			if (fn === '_connect') {
				if ("WebSocket" in window && (proto === 'auto' || proto === 'websockets')) {

					console.log('going with WEBSOCKETS', service);

					channel = new WebSocket("ws://" + 'localhost:8080/syncshare/wbs/' + service);
					channel.onopen = function() {
						reply(fn, null, Object.keys(remotes), dfid, true);
					};
					channel.onmessage = function(e) {
						if (e.data) {
							var data = e.data.split(" ", 2);
							reply(data[0], data[1], JSON.parse(e.data.substring(data[0].length+data[1].length+2)));
						}
					};
					channel.onerror = function() {
						// let the parent know that we're ready
						reply(fn, null, Object.keys(remotes), dfid, false);
					};
					channel.onclose = function() {
						console.log('Connection closed');
					};
				} else {
					console.log('going with SSE', service);
					
					channel = new EventSource('/syncshare/sse/' + service);
					channel.addEventListener('message', function(response) {
						var data = response.data.split(" ", 2);
						reply(data[0], data[1], JSON.parse(response.data.substring(data[0].length+data[1].length+2)));
					});

					// let the parent know that we're ready
					reply(fn, null, Object.keys(remotes), dfid, true);
				}
			}
			else
			if (remotes[fn]) {
				remotes[fn].apply({
					success: function(response) { reply(fn, null, response, dfid, true);  },
					fail:    function(response) { reply(fn, null, response, dfid, false); },
					ajax:    request
				}, data);
			} else {
				throw "No remote function "+fn;
			}
		} else

		// regular call
		// redirecting to opened channel

		if (channel && channel.send) {
			channel.send(fn + " " + JSON.stringify(data || ""));
		} else {
			new request('/syncshare/sse/' + service +'/'+ fn).post(data);
		}
	}, false);

    return {
        remote: remote
    };

})('twitter', 'sse');

Syncshare.remote({
    upload: function(arg1, arg2) {
        this.success({ok: true});
    }
});
