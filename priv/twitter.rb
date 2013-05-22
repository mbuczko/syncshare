require './syncshare'

Syncshare::Module.register(:host => 'localhost', :service => 'twitter') do
  message :observe do |msg, header|
    puts "got token -> #{msg[:token]} and payload -> #{msg[:payload]}"
    reply({:hash => "britney|spears", :amount => 10}, header)
  end

  message :upload do |msg|
    puts "got token -> #{msg[:token]} and payload -> #{msg[:payload]}"
    broadcast({:status => "ok|true", :url => "http://onet.pl"})
  end  
end
