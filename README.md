# Syncshare - RabbitMQ based instant notification system

This is a concept of bridge between browser and the backend workers connected each other via Server Sent Events or WebSockets. Having connection established the client side (browser) is able to send asynchronous request or pseudo-synchronous RPC command which will be processed by one of assigned worker processes. Response may be sent back to origin or to all clients (browsers) connected to service. Imagine this as a kind of broadcast response.

Following is a diagrams which explains connection between components:
