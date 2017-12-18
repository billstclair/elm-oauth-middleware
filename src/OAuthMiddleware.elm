----------------------------------------------------------------------
--
-- OAuthMiddleware.elm
-- Client side of an OAuth Authorization Code Grant Flow
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module OAuthMiddleware
    exposing
        ( Authorization
        , ResponseToken
        , TokenAuthorization
        , TokenState(..)
        , authorize
        , getAuthorization
        , getAuthorizations
        , locationToRedirectBackUri
        , receiveTokenAndState
        , use
        )

{-| Client side of OAuth Authorization Code Grant Flow.

Your top-level Elm program must be created with `Navigation.program` or `Navigation.programWithFlags`. You can ignore incoming `Location` changes, but your `init` function needs to pass the initial `Location` to `receiveTokenAndState`, in case you're here because of a redirect back from your OAuth callback server (at `redirectUri`).


# Types

@docs Authorization, TokenAuthorization, ResponseToken, TokenState


# Client-side functions

@docs getAuthorization, getAuthorizations
@docs locationToRedirectBackUri, authorize, receiveTokenAndState, use

-}

import Erl.Query as EQ
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Navigation exposing (Location)
import OAuth
import OAuth.AuthorizationCode
import OAuthMiddleware.EncodeDecode as ED
import Task exposing (Task)


{-| Configuration for sending a request to the authorization server.

`authorizationUri` and `tokenUri` are provided by the OAuth authentication service.

`authorize` sends a request to `authorizationUri`, encoding the other parameters into the `state` sent there. The `redirectUri` server, implemented by the `server` directory of this package, uses the `tokenUri` as a key into its table containing valid values for `redirectBackUri` and the client secret that will be sent to the `tokenUri`.

`clientId`, `redirectUri`, `scope`, and `state` are all standard parts of the OAuth protocol, except `state` is optional here, and encoded so it comes back just as it went out.

`clientId` is assigned by the OAuth resource. It also provides a list of possible `scope` values. You can use any subset of those.

`redirectUri` is sent to the OAuth authorization web server. If the user successfullly logs in, an authorization code will be sent there as a Uri query parameter.

`state` is any string you'd like. It is passed to the authorization web server, which passes it to the `redirectUri`, and it is passed back to your application via the `redirectBackUri`.

`redirectBackUri` is a Uri to redirect to AFTER the `redirectUri` exchanges the received code for an access token. It will resume your Elm application, this time with an access token in its hand.

-}
type alias TokenAuthorization =
    { authorization : Authorization
    , scope : List String
    , state : Maybe String
    , redirectBackUri : String
    }


{-| You'll usually get this from a JSON file, often via `getAuthorizations`.
-}
type alias Authorization =
    ED.Authorization


{-| Get a JSON file encoding an `Authorization`.

The JSON format is as follows. You'll change it with information from your OAuth provider, and for your redirect server, and store it in a convenient place on the same server serving your compiled Elm code.

The `scopes` field is an object, mapping your internal name each scope to the actual OAuth provider name. For most OAuth providers, the two will be identical, but Google, for example, uses long URL-looking strings for scope names, so it's convenient to have a shorter name your application can use. This field isn't used by any of the `OAuthMiddleware` code, except the example, so you can safely set it to `{}`, if you prefer to just encode the scope strings as constants in your Elm code.

Your client secret is stored with the redirect server, and never leaves that server machine.

    { "name": "Gmail",
      "authorizationUri": "https://accounts.google.com/o/oauth2/auth",
      "tokenUri": "https://accounts.google.com/o/oauth2/token",
      "clientId": "<Your OAuth clientid>
      "redirectUri": "<Your redirect server Uri>"
      "scopes": {"<Your scope name>": "<OAuth provider's scope name>"}
    }

-}
getAuthorization : String -> Http.Request Authorization
getAuthorization url =
    Http.get url ED.authorizationDecoder


{-| Get a JSON file encoding an `Authorization` list.
-}
getAuthorizations : String -> Http.Request (List Authorization)
getAuthorizations url =
    Http.get url ED.authorizationsDecoder


{-| Convert a `Navigation.Location` into a string suitable for the `redirectBackUri` in a `TokenAuthorization`.
-}
locationToRedirectBackUri : Location -> String
locationToRedirectBackUri location =
    location.protocol ++ location.host ++ location.pathname


{-| Send an authorization request.

This will cause the authorization server to ask the user to login. If successful, it will send the received code and `TokenAuthorization.state` to the `TokenAuthorization.authorization.redirectUri` for generation of a token to send back to the `TokenAuthorization.redirectBackUri`. Your code at that Uri will receive an encoded `ResponseToken` on the `responseTokenQuery` parameter, or an error string on the `responseTokenQueryError` parameter. Use `decodeResponseToken` to turn the `responseTokenQuery` string into an `ResponseToken`, which you can use to do authenticated requests, just as if you had called `OAuth.TokenAuthorizationCode.authenticate` yourself, but hiding the client secret on the redirect server.

The returned `Cmd` will cause the user's browser to navigate away from your app for authentication and token fetching by the `redirectUri`. The redirect server will navigate back with query args that you can process with `receiveTokenAndState`.

-}
authorize : TokenAuthorization -> Cmd msg
authorize { authorization, redirectBackUri, scope, state } =
    let
        { authorizationUri, tokenUri, clientId, redirectUri } =
            authorization
    in
    OAuth.AuthorizationCode.authorize
        { clientId = clientId
        , redirectUri = redirectUri
        , responseType = OAuth.Code
        , scope = scope
        , state =
            Just <|
                ED.encodeRedirectState
                    { clientId = clientId
                    , tokenUri = tokenUri
                    , redirectUri = redirectUri
                    , scope = scope
                    , redirectBackUri = redirectBackUri
                    , state = state
                    }
        , url = authorizationUri
        }


{-| An alias for `OAuth.ResponseToken`.
-}
type alias ResponseToken =
    OAuth.ResponseToken


{-| The result of parsing a Navigation.Location that may have come from
a redirect from a callback server, as implemented in the `server` directory
of this project.

For a `TokenAndState` or `TokenErrorAndState`, the state you passed is the `(Maybe String)`.

You will rarely need to look inside a `ResponseToken`. Just pass it to `use` to add it to the headers for a request to the protected resource.

`TokenDecodeError` means that there was a properly-named query string, but an error occurred while decoding it.

`NoToken` means that there was no query parameter that looked like a token or an error message about getting a token. In other words, this invocation of your webapp was not due to a redirection from the callback server.

-}
type TokenState
    = TokenAndState ResponseToken (Maybe String)
    | TokenErrorAndState String (Maybe String)
    | TokenDecodeError String
    | NoToken


{-| Parse a returned `ResponseToken` from a `Navigation.Location`
-}
receiveTokenAndState : Location -> TokenState
receiveTokenAndState location =
    let
        query =
            EQ.parse location.search

        states =
            EQ.getValuesForKey "state" query

        responses =
            EQ.getValuesForKey ED.responseTokenQuery query

        errors =
            EQ.getValuesForKey ED.responseTokenQueryError query
    in
    case ( responses, errors ) of
        ( response :: _, _ ) ->
            case ED.decodeResponseToken response of
                Ok token ->
                    TokenAndState token token.state

                Err msg ->
                    TokenDecodeError msg

        ( _, error :: _ ) ->
            TokenErrorAndState
                error
            <|
                case states of
                    s :: _ ->
                        Just s

                    _ ->
                        Nothing

        _ ->
            NoToken


{-| Use a token to add authenticatication to a request header.

A thin wrapper around `OAuth.use`.

-}
use : ResponseToken -> List Http.Header -> List Http.Header
use responseToken headers =
    OAuth.use responseToken.token headers
