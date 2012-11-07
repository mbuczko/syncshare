require "rubygems"
require "./syncshare"

class Twitter < Syncshare::Module

  # remote procedures

  def rpc_observe(payload)
    puts "Got an RPC call: #{payload}, routing key is 'observe'"
  end

  
end

Twitter.register(:service => "twitter", :rpc => ["observe"]).activate
  
