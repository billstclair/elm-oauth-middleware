const fs = require('fs')
const Http = require("http")
const Ews = require("./ews.js")
const App = require("./elm.js")

const worker = App.Main.worker()
const receiveFile = worker.ports.receiveFile

worker.ports.getFile.subscribe(function(filename) {
  fs.readFile(filename, 'utf8', function(err, data) {
    receiveFile.send(err ? null : data)
  })
})

var httpServer = Http.createServer(Ews.createRequestListener(worker))

httpServer.listen(3000, function () {
    console.log("listening at localhost:3000")
})
