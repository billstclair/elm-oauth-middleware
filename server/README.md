This directory contains the `redirectUri` server for the [billstclair/elm-oauth-middleware](http://package.elm-lang.org/packages/billstclair/elm-oauth-middleware/latest) package.

In order to run it on your server, first ensure that `node` and `npm` are installed, then do the following:

# Install

    cd ...
    git clone https://github.com/billstclair/elm-oauth-middleware.git
    cd elm-oauth-middleware
    npm install

# Configure

    cd .../elm-oath-middleware/server
    cp src/config.json.template src/config.json
    # Edit src/config.json with your configuration details.

# Build

This requires Elm. You may want to do it on your local server, and then upload `package.json` and the `build` directory to your server.

    cd .../elm-oauth-middleware/server
    npm run setup

# Run

    cd .../elm-oauth-middleware/server
    npm start


By default, the server runs on port 3000, but you can change that in the configuration file. You may want to block that port to the outside world, and configure Apache or Nginx to reverse proxy port 80 for that port at some PATH.

For example, here's the line in the Apache site file for my server:

    <VirtualHost *:443>
        ServerName example.com
        ...
        ProxyPass "/oaath/" "http://localhost:3000/"
    </VirtualHost>

# Create server image

    cd .../elm-oauth-middleware/server
    npm run build
    bin/populate-site
    # Copy the site directory to your server machine.
    # Create `config.json` as instructed below.
    cd .../<the site directory>
    npm install # once
    npm start

# config.json

The server is configured by a file named `config.json` in its `build` directory. The `bin/populate-site` script copies the distribution file named `config.json.template` into `site/build`, but you need to create `config.json` yourself, on the server, starting with `config.json.template` as a template. Example:

    [{"comment","Any configuration with a comment is ignored."
     },
     {"port": 3000,
      "configSamplePeriod": 2
     },
     {"tokenUri": "https://example.com/oath/token",
      "clientId": "clientid",
      "clientSecret": "secret",
      "redirectBackHosts": ["https://example.com", "oauth-client-dev.com"]
     }
    ]
    
The `port` and `configSamplePeriod` object is optional, and may appear only once. It sets the TCP port to listen on, and the time between probes looking for updates to `config.json`. Both `port` and `configSamplePeriod` are optional, and default to the values shown above. If `configSamplePerios` is less than or equal to zero, the server will NOT probe for updates, and you'll have to stop and restart it to read an updated `config.json`.

`tokenUri` and `clientId` are included in the `Authorization` fetched by `OAuthMiddleware.getAuthorization`. `redirectBackHosts` is a list of acceptable hosts for the `redirectUri` field in an `Authorization`. If a host name is prefixed with `https://`, then the incoming `redirectBackUri` must also be for the `https` protocol.

The server hot-loads changes to `config.json`, so to change the configuration, just edit that file, and watch the server output to see that it successfully parsed the new configuration (I run my server in an Emacs shell, and run that emacs inside of a `screen` process). 
    
# Build, populate, and upload server image

If you have my [`rsyncit` script](https://github.com/billstclair/wws-scripts/blob/master/bin/rsyncit) in your `PATH`, and you create a `.sshdir` file [as instructed](https://github.com/billstclair/wws-scripts#rsyncit) in the `site` directory, you can build, populate, and upload the server image with one script:

    cd .../elm-oauth-middleware/server
    bin/update-site

This will NOT overwrite `config.json`.

# GitHub Accomodation

This section is mostly for me, so you can safely ignore it.

I discovered while debugging the server, that GitHub's OAuth2 token server is not standard compliant. Unless you send it an `Accept: application/json` header, it URL-encodes the returned token, instead of sending it as JSON in the body. And they encode the returned `scope` as a comma-separated string instead of as the specified JSON array of strings (actually, the spec for Authorization Code grant flow doesn't specify any return at ALL for `scope` or `state`, nor does it require them as input, though they ARE returned by the Implicit grant flow token request).

I fixed this in `truqu/elm-oauth2`, and submitted a [pull request](https://github.com/truqu/elm-oauth2/pull/3), but they, understandably, decided not to pollute their code with a one-vendor work-around. I just want the server to work with GitHub, so I copied the necessary code for their [`Internal`](https://github.com/truqu/elm-oauth2/blob/master/src/Internal.elm)`.authenticate` function into `src/OAuthTokenServer/Authenticate.elm`, and added my patch there.

GitHub's documentaton of their token server's behavior is [here](https://developer.github.com/apps/building-oauth-apps/authorization-options-for-oauth-apps/#response).

Here are links to IETF RFC 6749 section [4.1.3. Access Token Request](https://tools.ietf.org/html/rfc6749#section-4.1.3) and section [4.1.4. Access Token Response](https://tools.ietf.org/html/rfc6749#section-4.1.4).
