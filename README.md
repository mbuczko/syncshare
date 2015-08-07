# Syncshare - RabbitMQ based instant notification system

This is a concept of bridge between http clients and the backend workers basing on pubsub (rabbitMQ in this case) and cowboy server holding all WebSocket- and Server Sent Events connections alive. Having connection established the client side (browser) is able to send asynchronous request or synchronous RPC command which will be processed by one of assigned worker processes. 

Response may be sent back to origin or to all clients (browsers) connected to given service. You may imagine this kind of response as broadcast one.

Following is a diagrams which explains connection between components:

![connections](https://github.com/mbuczko/syncshare/blob/master/syncshare.png "connections")

## Usage

To be able to use SyncShare on client side, a javascript client library should be added:

    <script type="text/javascript" src="js/client.js"></script>
    
Now it's time to define a SyncShare client and connect to the service:

    new Syncshare.Client('http://my-synshare-service.com', {transport: 'websockets'}).connect(function(session) {
        ...
    });
    
With no surprise, possible transports are: ```websockets``` and ```sse```.

Having connection established and session ready we may try to call some rpc functions:

    session.rpc().comment({comment: 'lorem ipsum', userId: 123})
		  .then(
				function(success) {
					console.log('SUCCESS', success);
				},
				function(error) {
					console.log('ERROR', error);
				});

Or we may listen to certain events coming from workers:

    session.on('comment', function(data, isBroadcast) {
				console.log(data, 'BROADCAST', isBroadcast);
			})
			.on('upload', function(data, isBroadcast) {
				console.log(data, 'BROADCAST', isBroadcast);
			});
