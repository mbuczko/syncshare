require 'syncshare'

Syncshare::Module.register(:host => 'localhost', :service => 'twitter') do
  message :observe do |msg, header|
    puts "got token -> #{msg[:token]} and data -> #{msg[:data]}"
    reply :direct, {:hash => "srasz", :amount => 10}, header
  end

  message :upload do |msg, header|
    puts "got token -> #{msg[:token]} and data -> #{msg[:data]}"
    reply :public, {:status => "ok true", :url => "http://linkify.me"}, header
  end

  message :comment do |msg, header|
    reply [2,3], {:status => "ok true", :url => "http://linkify.me"}, header
  end
end
