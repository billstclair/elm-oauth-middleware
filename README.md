[![elm-package](https://img.shields.io/badge/elm-1.0.0-blue.svg)](http://package.elm-lang.org/packages/billstclair/elm-oauth-middleware/latest)

This package implements the client side of a complete OAuth [Authorization Code](https://tools.ietf.org/html/rfc6749#section-1.3.1) Grant Flow.

The grant flow requires a redirect server. That is also part of this package.

How it works:

Your Elm client software contacts the authorization server, via the `OAuthMiddleware` module, passing the `clientId`, `<redirectUri>`, and `<state>`. That sends the customer to the server web site to authorize the OAuth connection. If she successfully logs in, the authorization server redirects to the `<redirectUri>` with:

    <redirectUri>?code=<long hex string>&state=<state>
    
The redirect server passes the code, the client id, and the client secret (obtained from configuration parameters stored on the server machine) to the token server, gets the authorization `<token>` in its response, and redirects the browser back to the client at a Uri encoded in the `<state>`, encoding the `<token>` with that Uri.

The server configuration determines which redirect-back domains are allowed in the `<state>` and their corresponding client IDs and client secrets. This prevents the server from being used by all and sundry to piggy-back on top of its client IDs and client secrets to gain authorization.

The [`server` directory](https://github.com/billstclair/elm-oauth-middleware/tree/master/server) contains the server code, which you must configure and run at a `<redirectUri>` that you've associated with your client ID(s) and client secret(s). See its README file for configuration instructions.

The `src` directory and this directory's `elm-package.json` implement the client side of the conversation, enabling initiating of the OAuth connection and processing the redirect when it comes back from the server.

The server code is an adpatation of Asger Nelson's [`elm-web-server`](https://www.npmjs.com/package/elm-web-server) package. In particular, it was based off of his [hello-world](https://github.com/opvasger/elm-web-server/tree/master/examples/hello-world) example, with the WebSocket code removed.

It implements a web server, which runs in [Node.js](https://nodejs.org/en/), and behaves as a redirect server for the grant flow. It can be configured to operate as redirect server for a number of different applications served from a number of different hosts, including fake hosts, established through /etc/hosts on your development machine. The latter enables local development of client software, while using the server to do authorization during testing.
