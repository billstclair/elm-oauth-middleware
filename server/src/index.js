var Http = require("http")
var Ews = require("./ews.js")
var App = require("./elm.js")

var worker = App.Main.worker()

var httpServer = Http.createServer(Ews.createRequestListener(worker))

httpServer.listen(3000, function () {
    console.log("listening at localhost:3000")
})
