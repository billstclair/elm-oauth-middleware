This directory contains the `redirectUri` server for the [billstclair/elm-oauth-middleware](http://package.elm-lang.org/packages/billstclair/elm-oauth-middleware/latest) package.

In order to run it on your server, first ensure that `node` and `npm` are installed, then do the following:

# Install

    cd ...
    git clone https://github.com/billstclair/elm-oauth-middleware.git
    cd elm-oauth-middleware
    npm install xhr2

# Configure

    cd .../elm-oath-middleware/server
    cp src/config.json.template src/config.json
    # Edit src/config.json with your configuration details.

# Build

This required Elm. You may want to do it on your local server, and then upload `package.json` and the `build` directory to your server.

    cd .../elm-oauth-middleware/server
    npm run setup

# Run

    cd .../elm-oauth-middleware/server
    npm start


The server runs on port 3000. That number is wired in to `src/index.js`. You may not want to use that port for the outside world, so you may want to configure Apache or Nginx to reverse proxy port 80 for that port on some URL.

# Create server image

    cd .../elm-oauth-middleware/server
    npm run build
    bin/populate-site
    # Copy the site directory to your server machine
    # Make sure you `npm install xhr2` there.
    # Run `npm start` in that directory.
    
# Build, populate, updload server image

If you have my [`rsyncit` script](https://github.com/billstclair/wws-scripts/blob/master/bin/rsyncit) in your `PATH`, and you create a `.sshdir` file [as instructed](https://github.com/billstclair/wws-scripts#rsyncit) for the `site` directory, you can build, populate, and upload the server image with one script:

    cd .../elm-oauth-middleware/server
    bin/update-site
