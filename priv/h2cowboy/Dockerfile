FROM erlang:24.0

COPY . .

RUN make

EXPOSE 7070

CMD trap 'exit' INT; ./_rel/websocket_example/bin/websocket_example foreground
