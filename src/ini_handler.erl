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

    var init = function(service) {
console.log('listening to service '+service);
        var es = new EventSource('/syncshare/'+service);

        // bubble message up to parent window

        es.addEventListener('msg',function(msg) {
            window.parent.postMessage({reply: msg.data}, '*');
        });

        // delegate RPC messages to rpc queue

        window.addEventListener('message', function(e) {
            this.req = this.req || new xhr();
            this.req.connect('/syncshare/'+ e.data.service + '/rpc/'+e.data.call, e.data.params, function() {

            });
        }, false);
    };
    init('~s');
}(window);</script>", [Service]), Req),
	{ok, Req2, State}.

terminate(_Req, _State) ->
	ok.

