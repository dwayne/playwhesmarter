require 'sinatra'

get '/' do
  erb :index, layout: false
end
