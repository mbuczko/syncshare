require "rubygems"
require "./syncshare"

class Twitter < Syncshare::Module

  # remote procedures

  def direct_observe(payload, header)
    reply({
            :hash => "#britney",
            :amount => 10
          }, header)
  end

  def direct_upload(payload, header)
    reply_all({
            :status => "ok",
            :url => "http://onet.pl"
          }, header)
  end
  
end

Twitter.register(:service => "twitter", :messages => ["observe", "upload"]).activate
  
