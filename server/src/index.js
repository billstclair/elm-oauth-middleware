const fs = require('fs')
const Http = require("http")
const Ews = require("./ews.js")
const App = require("./elm.js")

const worker = App.OAuthTokenServer.worker()
const receiveFile = worker.ports.receiveFile

worker.ports.getFile.subscribe(function(filename) {
  fs.readFile(filename, 'utf8', function(err, data) {
    receiveFile.send(err ? null : data)
  })
})

worker.ports.httpListen.subscribe(function(port) {
  httpListen(port);
});

var httpServer = Http.createServer(Ews.createRequestListener(worker))
var listenPort = 1/0;           // we never get infinities from the Elm code.

httpServer.on('error', function(e) {
  console.log('Http error: ' + e);
});

function httpListen(port) {
  if (port != listenPort) {
    if (listenPort > 0) {
      listenPort = 1/0;
      if (httpServer.listening) {
        httpServer.close(function () {
          httpListen(port);
        });
        return;
      }
    }
    listenPort = port;
    if (port > 0) {
      httpServer.listen(port, function(err) {
        // Condition avoids extraneous message after EADDRINUSE error
        if (port == listenPort) {
          console.log("listening at localhost:" + port);
        }
      });
    } else {
      console.log("HTTP listener shut down.");
    }
  }
}
