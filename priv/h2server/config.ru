#!/usr/bin/env -S falcon serve --bind "http://0.0.0.0:7070" --count 1 -c

require 'async/websocket/adapters/rack'
require 'set'

$connections = Set.new

run lambda {|env|
 Async::WebSocket::Adapters::Rack.open(env) do |connection|
   $connections << connection

   while message = connection.read
     $connections.each do |connection|
       connection.write(message)
       connection.flush
     end
   end
 ensure
   $connections.delete(connection)
 end or [200, {}, ["Hello World"]]
}
