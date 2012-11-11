module Syncshare
  require 'amqp'
  require 'json'

  class Module
    attr_accessor :options

    class << self

      def register(params)
        instance = self.new
        options  = {
          :host     => "localhost",
          :messages => []
        }

        [:service, :host, :messages].each do |key|
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

        @exchange_public  = @channel.fanout(@service + "-public")

        puts "Connecting to AMQP broker. Running #{AMQP::VERSION} version of the gem..."

        if options[:messages].length > 0
          @exchange_direct  = @channel.topic(@service + "-direct")

          options[:messages].each do |message|
            @channel.queue("rpc." + message).bind(@exchange_direct, :routing_key => "rpc."+message).subscribe do |header, payload|
              proc = header.routing_key.sub('.', '_')
              send(proc, JSON.parse(payload), header) if self.class.method_defined? proc
            end
          end
        end        
      end
    end

    def reply(payload, header)
      @exchange_direct.publish(payload, :routing_key => header.reply_to, :correlation_id => header.correlation_id)
    end

    def reply_all(payload)
      @exchange_public.publish(payload)
    end

  end
end
