require 'sinatra'
require 'omniauth-twitter'

set :port, 3000

use OmniAuth::Builder do
  provider :twitter, 'w3yv46Z0mowGFcMhEAregQ', 'UEs8OFeGvHKZFavWwh7vth8w8sW8gh0BtxbzA98M'
end

configure do
  enable :sessions
end

helpers do
  def current_user
    session[:current_user]
  end
end

get '/login' do
  redirect to("/auth/twitter")
end

get '/auth/twitter/callback' do
  env['omniauth.auth'] ? session[:current_user] = env['omniauth.auth']['info'] : halt(401,'Not Authorized')
  redirect '/'
end

get '/auth/failure' do
  params[:message]
end

get '/' do
  puts current_user
  
  if current_user.nil?
    "You're not logged in. <a href='/login'>Please, login via Twitter.</a>."
  else
    erb :index
  end
end

get '/login' do
  session[:current_user] = true
  "You are now logged in"
end

get '/logout' do
  session[:current_user] = nil
  "You are now logged out"
end
