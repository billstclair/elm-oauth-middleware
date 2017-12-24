[![elm-package](https://img.shields.io/badge/elm-1.0.2-blue.svg)](http://package.elm-lang.org/packages/billstclair/elm-oauth-middleware/latest)
[![Build Status](https://travis-ci.org/billstclair/elm-oauth-middleware.svg?branch=master)](https://travis-ci.org/billstclair/elm-oauth-middleware)

This package implements the client side of a complete OAuth [Authorization Code](https://tools.ietf.org/html/rfc6749#section-1.3.1) Grant Flow.

The grant flow requires a redirect (callback) server. That is also included in this package, but you must set it up on a server machine running Node.js. One server can handle multiple OAuth authorization services and webapps requiring authorization.

The Authorization Code grant flow is usually used for server-based web sites, where all the communication with the OAuth authorization and token server, and with the authenticated site, happens on the web server. This package allows you to do all but the token server communication in your Elm code, in the browser.

Many OAuth-verified services provide one of the client-only grant flows. Those are supported directly by the [`truqu/elm-oauth2`](http://package.elm-lang.org/packages/truqu/elm-oauth2/latest) package, and don't require this package.

# How it works

Your Elm client software contacts the authorization server, via the `OAuthMiddleware` module, passing the `clientId`, `<redirectUri>`, and `<state>`. That sends the customer to the server web site to authorize the OAuth connection. If she successfully logs in, the authorization server redirects to the `<redirectUri>` with:

    <redirectUri>?code=<long hex string>&state=<state>
    
The redirect server passes the code, the client id, and the client secret (obtained from configuration parameters stored on the server machine) to the token server, gets the authorization `<token>` in its response, and redirects the browser back to the client at a Uri encoded in the `<state>`, encoding the `<token>` with that Uri.

The server configuration determines which redirect-back domains are allowed in the `<state>` and their corresponding client IDs and client secrets. This prevents the server from being used by all and sundry to piggy-back on top of its client IDs and client secrets to gain authorization.

# Server details

The [`server` directory](https://github.com/billstclair/elm-oauth-middleware/tree/master/server) contains the server code, which you must configure and run at a `<redirectUri>` (AKA callback Uri) that you've associated, via your OAuth authorization service(s), with your client ID(s) and client secret(s). See its README file for configuration instructions.

The `src` directory and this directory's `elm-package.json` implement the client side of the conversation, enabling initiating of the OAuth connection and processing the redirect when it comes back from the server.

The server code is an adpatation of Asger Nelson's [`elm-web-server`](https://www.npmjs.com/package/elm-web-server) package. In particular, it was based off of his [hello-world](https://github.com/opvasger/elm-web-server/tree/master/examples/hello-world) example, with the WebSocket code removed.

It implements a web server, which runs in [Node.js](https://nodejs.org/en/), and behaves as a redirect server for the grant flow. It can be configured to operate as redirect server for a number of different applications served from a number of different hosts.

# CORS

There are [Cross-Origin Resource Sharing](https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS) issues any time a web client communicates with a host other than the one that supplied its HTML. The OAuth providers I've tried all handle this properly, but if you successfully get a token, and then receive a `NetworkError` when you try to do a REST request, there may be a CORS problem, and you'll have to either convince the service provider to fix it, or move your code to the server.

You can verify this in the `Network` tab of your web browser's developer tools. There will be an `OPTIONS` request, and the `Response Headers` will be missing one or more of (matching) `access-control-allow-methods`, `access-control-allow-origin`, or `access-control-allow-headers: authorization`.

# Development

During development, it's nice to be able to use your local machine for the client code, but still let the server get access tokens. You can do this by inventing a non-existent domain (anything you never use on the real web), and adding it to `/etc/hosts`:

    127.0.0.1 oauth-client-dev.com
    
Then you include `oauth-client-dev.com` in the `redirectBackHosts` in the server's `config.json` file.

You must start `elm-reactor` with your fake host:

    elm reactor -a "oauth-client-dev.com"
    
And aim your browser at it:

    http://oauth-client-dev.com:8000

# Warning

Unfortunately, that nifty development fake domain can be used by a hacker to mint tokens using your server. He'll have to take the `authorizationUri`, `tokenUri`, `clientId`, and `redirectUri` from your code or your loading of `authorizations.json`, but that isn't rocket science with modern web browser developer tools, so he can do it.

He'll still need to have login credentials for the service your client ID and secret unlocks, so any spamming he does will have that account's initials on it. But it will also have your OAuth app's initials on it, so could cause the OAuth provider to censure your account.

I find this risk to be acceptable. You'll need to decide that you do before using this package.
