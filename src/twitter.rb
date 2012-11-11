require "rubygems"
require "./syncshare"

class Twitter < Syncshare::Module

  # remote procedures

  def rpc_observe(payload, header)
    reply({
            :ack => true,
            :dupa => "jasia"
          }, header)
  end

  
end

Twitter.register(:service => "twitter", :messages => ["observe"]).activate
  
