var Crypto = require("crypto")

global.XMLHttpRequest = require("xhr2").XMLHttpRequest

var createRequestListener = function (worker) {

    if (!worker || !worker.ports)
        throw Error("Invalid configuration - Ensure you are passing an instantiated Elm-worker to 'createRequestListener'.")

    if (!worker.ports.httpIn || !worker.ports.httpOut)
        throw Error("Invalid configuration - Ensure the worker you are passing to 'createRequestListener' is utilizing the 'Server.Http'-module.")

    var unresolved = {}

    worker.ports.httpOut.subscribe(function (output) {

        if (!unresolved[output.id])
            return console.error("no unresolved request with id: " + id)

        unresolved[output.id].writeHead(output.status.code, output.status.message, output.headers)

        unresolved[output.id].end(output.body)

        delete unresolved[output.id]
    })

    return function (request, response) {

        Crypto.randomBytes(16, function (error, buffer) {

            if(error) {
                console.error(error)
                return response.end()
            }

            var id = buffer.toString("hex")

            unresolved[id] = response

            var body = []

            request
                .on("data", function (chunk) {
                    body.push(chunk)
                })
                .on("error", function (error) {
                    console.error(error)
                    response.end()
                })
                .on("end", function () {

                    worker.ports.httpIn.send({
                        id: id,
                        url: request.url,
                        method: request.method,
                        headers: request.headers,
                        body: Buffer.concat(body).toString()
                    })
                })
        })
    }
}

var attachMessageListener = function (worker, server) {

    if (!worker || !worker.ports)
        throw Error("Invalid configuration - Ensure you are passing an instantiated Elm-worker to 'attachMessageListener'.")

    if (!worker.ports.websocketIn || !worker.ports.websocketOut)
        throw Error("Invalid configuration - Ensure the worker you are passing to 'attachMessageListener' is utilizing the 'Server.WebSocket'-module.")

    var connections = {}

    var dropDisconnected = function () {

        Object.getOwnPropertyNames(connections).forEach(function (id) {

            if(!connections[id].isAlive) {

                worker.ports.websocketIn.send({ disconnected: id })

                connections[id].terminate()

                delete connections[id]
            } else {

                connections[id].isAlive = false

                connections[id].ping("", false, true)
            }
        })

        setTimeout(dropDisconnected, 30000)
    }

    dropDisconnected()

    worker.ports.websocketOut.subscribe(function (output) {

        if (output.to && output.message) {
        
            if (!connections[output.to])
                return console.error("no connection with id: " + output.to)
            
            connections[output.to].send(output.message, function (error) {
            
                if (error)
                    console.error(error)
            })
        } else if (output.diconnect) {

            if (!connections[output.disconnect])
                return console.error("no connection with id: " + output.disconnect)

            worker.ports.websocketIn.send({ disconnected: output.disconnect })
                
            connections[output.disconnect].terminate()
            
            delete connections[output.disconnect]
        } else {

            console.error("message not recognized: " + output)
        }
    })

    server.on("connection", function (connection) {
        
        connection.isAlive = true

        connection.on("pong", function () {
            connection.isAlive = true
        })

        Crypto.randomBytes(16, function (error, buffer) {
        
            var id = buffer.toString("hex")

            connections[id] = connection
            
            worker.ports.websocketIn.send({ connected: id })

            connection.on("message", function (message) {

                worker.ports.websocketIn.send({ from: id, message: message })
            })
            
            connection.on("error", function (error) {

                console.error(error)

                worker.ports.websocketIn.send({ disconnected: id })

                connections[id].terminate()

                delete connections[id]
            })

            connection.on("close", function () {

                worker.ports.websocketIn.send({ disconnected: id })

                delete connections[id]
            })
        })
    })
}

module.exports = {
    createRequestListener: createRequestListener,
    attachMessageListener: attachMessageListener
}
