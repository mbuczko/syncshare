module Syncshare
  require 'amqp'
  require 'json'

  class Module
    attr_accessor :options

    class << self

      def register(params)
        instance = self.new
        options  = {
          :host    => "localhost",
          :public  => [],
          :private => [],
          :rpc     => []
        }

        [:service, :host, :rpc].each do |key|
          options[key] = params[key] if params.include? (key)
        end

        instance.options = options
        return instance
      end

      def handlers(*calls)
        puts "been here"
        calls.each do |call|
          if superclass.responds_to(call)
            handle(call.to_s)
          end
        end
      end

    end

    def activate
      EventMachine.run do
        @service = @options[:service]
        @connection = AMQP.connect(:host => @options[:host])
        @channel  = AMQP::Channel.new(@connection)

        puts "Connecting to AMQP broker. Running #{AMQP::VERSION} version of the gem..."

        if options[:public].length > 0
          exchange_public  = @channel.fanout(@service + "-public")
        end

        if options[:private].length > 0
          exchange_private  = @channel.direct(@service + "-private")
        end

        if options[:rpc].length > 0
          exchange_rpc = @channel.topic(@service + "-rpc")

          options[:rpc].each do |message|
            @channel.queue(@service + "." + message).bind(exchange_rpc, :routing_key => message).subscribe do |key, payload|
              proc = 'rpc_'+key.routing_key
              if self.class.method_defined? proc
                send(proc, JSON.parse(payload))
              end
            end
          end
        end        
      end
    end

  end
end
