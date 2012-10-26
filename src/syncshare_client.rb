require "rubygems"

require 'amqp'

EventMachine.run do
  connection = AMQP.connect(:host => '127.0.0.1')
  puts "Connecting to AMQP broker. Running #{AMQP::VERSION} version of the gem..."

  channel  = AMQP::Channel.new(connection)
  exchange = channel.fanout("twitter/public")

  # queue    = channel.queue("amqpgem.examples.hello_world", :auto_delete => true)
  # queue.subscribe do |payload|
  #   puts "Received a message: #{payload}. Disconnecting..."

  #   connection.close {
  #     EventMachine.stop { exit }
  #   }
  # end
  ["Hello", "Cruel", "Word"].each {|message| exchange.publish "#{message}"}
  
end
