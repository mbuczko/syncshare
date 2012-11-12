%% @doc SSE initialization handler.
-module(ini_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/2]).


init(_Transport, Req, Opts) ->
	{ok, Req, undefined_state}.

handle(Req, State) ->
    {Service, _} = cowboy_req:binding(service, Req),
	{ok, Req2} = cowboy_req:reply(200, [], io_lib:format("<script>

var Syncshare = Syncshare || {};
Syncshare.Proxy = function(window, undefined) {

    var xhr = function() {
        var xhr, completed;
        try { xhr = new ActiveXObject('Msxml2.XMLHTTP'); }
        catch (e) {
            try { xhr = new ActiveXObject('Microsoft.XMLHTTP'); }
            catch (e) {
                try { xhr = new XMLHttpRequest(); }
                catch (e) { return null; }}}

        this.connect = function(url, params, callback) {
            completed = false;
            try {
                xhr.open('POST', url, true);
                xhr.setRequestHeader('Method', 'POST '+url+' HTTP/1.1');
                xhr.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');

                xhr.onreadystatechange = function() {
                    if (xhr.readyState == 4 && !completed) {
                        completed = true;
                        callback(xhr);
                    }};
                xhr.send('body='+escape(params));
            }
            catch(z) { return false; }
            return true;
        };
        return this;
    };

    var msg = function(message) {
        window.parent.postMessage(message, '*');
    };

    var init = function(service) {
        var es = new EventSource('/syncshare/'+service);

        es.addEventListener('error', function(e) {
            if (e.readyState == EventSource.CLOSED) {
                console.log('syncshare: connection closed');
            }
        }, false);

        // bubble message up to parent window

        es.addEventListener('rpc',function(reply)    { msg({rpc: reply.data}); });
        es.addEventListener('public',function(reply) { msg({broadcast: reply.data}); });

        // delegate RPC messages to rpc queue

        window.addEventListener('message', function(e) {
            this.req = this.req || new xhr();
            this.req.connect('/syncshare/'+ e.data.service + '/rpc/'+e.data.call, e.data.params, function() {
                console.log('syncshare: successfully pushed event to /rpc/'+e.data.call);
            });
        }, false);
    };
    init('~s');
}(window);</script>", [Service]), Req),
	{ok, Req2, State}.

terminate(_Req, _State) ->
	ok.

