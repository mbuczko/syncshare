%% @doc SSE initialization handler.
-module(frame_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).


init(_Transport, Req, _Opts) ->
	{ok, Req, undefined_state}.

handle(Req, State) ->
    {Service, _} = cowboy_req:binding(service, Req),
    {ok, Req2} = cowboy_req:reply(200, [], io_lib:format("
<script>
var Syncshare = Syncshare || {};
Syncshare.Proxy = function(window, undefined) {

    var Request = function(url) {
        var xhr;

        try { 
            xhr = new ActiveXObject('Msxml2.XMLHTTP'); 
        }
        catch (e) {
            try { 
                xhr = new ActiveXObject('Microsoft.XMLHTTP'); 
            }
            catch (e) {
                xhr = new XMLHttpRequest(); 
            }
        }
        this.post = function(params) {
            xhr.open('POST', url, true);
            xhr.setRequestHeader('Method', 'POST '+url+' HTTP/1.1');
            xhr.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');
            xhr.send('body='+escape(params));
        };
    };

    var init = function(service) {
        var esr = new EventSource('/syncshare/sse/'+service),
            msg = function(type, response) {
                response = response.split('|');

                // update current queue name
                tok = response[0];

                // send the message back to parent frame
                window.parent.postMessage({type: type, payload: JSON.parse(response[1])}, '*');
            }, 
            tok;


        // bubble message up to parent window

        esr.addEventListener('direct', function(reply)     { msg('direct', reply.data); });
        esr.addEventListener('broadcast', function(reply)  { msg('broadcast', reply.data); });
        esr.addEventListener('connection', function(reply) { tok = reply.data; console.log('changed queue', tok)});

        // delegate direct messages to direct queue

        window.addEventListener('message', function(e) {
            new Request('/syncshare/sse/' + e.data.service + '/'+tok+'/' + e.data.call).post(e.data.params);
        }, false);
    };
    init('~s');
}(window);
</script>", [Service]), Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.

