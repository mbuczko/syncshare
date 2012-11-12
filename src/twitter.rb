require "rubygems"
require "./syncshare"

class Twitter < Syncshare::Module

  # remote procedures

  def rpc_observe(payload, header)
    reply_all({
            :ack => true,
            :dupa => "jasia"
          }, header)
  end

  
end

Twitter.register(:service => "twitter", :messages => ["observe"]).activate
  
