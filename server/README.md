This directory contains the `redirectUri` server for the [billstclair/elm-oauth-middleware](http://package.elm-lang.org/packages/billstclair/elm-oauth-middleware/latest) package.

In order to run it on your server, first ensure that `node` and `npm` are installed, then do the following:

# Install
cd ...
git clone https://github.com/billstclair/elm-oauth-middleware.git

# Configure
cd .../elm-oath-middleware/server
cp src/Config.elm.template src/Config.elm
# Edit src/Config.elm with your configuration details.

# Build
cd .../elm-oauth-middleware/server
npm run-script setup

# Run
cd .../elm-oauth-middleware/server
npm run-script
