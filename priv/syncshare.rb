module Syncshare
  require 'amqp'
  require 'json'

  class Module
    attr_accessor :options

    def self.register(params, &block)
      instance = self.new
      options  = { :host => "localhost" }

      [:service, :host].each do |key|
        options[key] = params[key] if params.include? (key)
      end

      instance.options = options

      # run event machinery
      instance.activate(DSL.new(block))
      
    end

    def activate(dsl)
      EventMachine.run do
        service = @options[:service]
        connection = AMQP.connect(:host => @options[:host])
        channel = AMQP::Channel.new(connection)

        puts "Connecting to AMQP broker at #{@options[:host]}[service=#{service}]. Running #{AMQP::VERSION} version of the gem..."

        dsl.exchange_public = channel.fanout(service + "-public")
        dsl.exchange_direct = channel.topic(service + "-direct")

        dsl.callers.keys.each do |key|
          channel.queue(service + ".direct-" + key.to_s).bind(dsl.exchange_direct, :routing_key => key).subscribe do |header, payload|
            token, load = payload.split('.')
                        
            proc = dsl.callers[key]

            if !proc.nil?
              proc.call({:token => token, :payload => JSON.parse(load)}, header)
            end
          end
        end
      end
    end
    
    class DSL
      attr_reader :callers
      attr_accessor :exchange_public, :exchange_direct
                 
      def initialize(block)
        @callers = {}
        instance_eval(&block)
      end
      
      def message(name, &block)
        @callers[name] = block
      end

      def reply(payload, header)
        exchange_direct.publish(payload.to_json,
                                 :routing_key => header.reply_to,
                                 :correlation_id => header.correlation_id,
                                 :headers => {:type => 'message'})
      end

      def broadcast(payload)
        exchange_public.publish(payload.to_json, :headers => {:type => 'broadcast'})
      end
      
    end
  end
end
