// see https://github.com/jasnell/http2ws/blob/master/http2ws.js

const http2 = require('http2')
const WebSocket = require('ws')

function createWebSocket(stream, headers) {
  // Take the Http2Stream object and set it as the "socket" on a new
  // ws module WebSocket object.. then wrap that using the websocket-stream...
  // It's not quite a perfect fit yet... but we have to start somewhere to
  // prove it works.
  stream.setNoDelay = function() {}   // fake it for now...
  const ws = new WebSocket(null)
  ws.setSocket(stream, headers, 100 * 1024 * 1024)
  return websocket(ws)
}

http2.createServer({ settings: { enableConnectProtocol: true } })
  .on('stream', (stream, headers) => {
    if (headers[':method'] == 'CONNECT') {
      stream.respond({ 'sec-websocket-protocol': headers['sec-websocket-protocol'] })

      const ws = createWebSocket(stream, headers)
      ws.pipe(ws)
    } else {
      stream.respond()
      stream.end('ok\n')
    }
  })
  .listen(7070, () => {
    console.log('H2 WebSocket server running on http://0.0.0.0:7070')
  })
