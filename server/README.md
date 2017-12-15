This directory contains the `redirectUri` server for the [billstclair/elm-oauth-middleware](http://package.elm-lang.org/packages/billstclair/elm-oauth-middleware/latest) package.

In order to run it on your server, first ensure that `node` and `npm` are installed, then do the following:

# Install
cd ...
git clone https://github.com/billstclair/elm-oauth-middleware.git

# Configure
cd .../elm-oath-middleware/server
cp src/config.json.template src/config.json
# Edit src/config.json with your configuration details.

# Build
cd .../elm-oauth-middleware/server
npm run setup

# Run
cd .../elm-oauth-middleware/server
npm start

The server runs on port 3000. That number is wired in to `src/index.js`. You may not want to use that port for the outside world, so you may want to configure Apache or Nginc to reverse proxy port 80 for that port on some URL.
