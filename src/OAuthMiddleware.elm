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
        , TokenState
        , authorize
        , receiveTokenAndState
        , use
        )

{-| Client side of OAuth Authorization Code Grant Flow.

Your top-level Elm program must be created with `Navigation.program` or `Navigation.programWithFlags`. You can ignore incoming `Location` changes, but your `init` function needs to pass the initial `Location` to `receiveTokenAndState`, in case you're here because of a redirect back from your OAuth callback server (at `redirectUri`).


# Types

@docs Authorization, TokenState


# Client-side functions

@docs authorize, receiveTokenAndState, use

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
type alias Authorization =
    { authorizationUri : String
    , tokenUri : String
    , clientId : String
    , redirectUri : String
    , scope : List String
    , state : Maybe String
    , redirectBackUri : String
    }


{-| Send an authorization request.

This will cause the authorization server to ask the user to login. If successful, it will send the received code and `Authorization.state` to the `Authorization.redirectUri` for generation of a token to send back to the `Authorization.redirectBackUri`. Your code at that Uri will receive an encoded `OAuth.ResponseToken` on the `responseTokenQuery` parameter, or an error string on the `responseTokenQueryError` parameter. Use `decodeResponseToken` to turn the `responseTokenQuery` string into an `OAuth.ResponseToken`, which you can use to do authenticated requests, just as if you had called `OAuth.AuthorizationCode.authenticate` yourself, but hiding the client secret on the redirect server.

The returned `Cmd` will cause the user's browser to navigate away from your app for authentication and token fetching by the `redirectUri`. The redirect server will navigate back with query args that you can process with `receiveTokenAndState`.

-}
authorize : Authorization -> Cmd msg
authorize { authorizationUri, tokenUri, clientId, redirectUri, redirectBackUri, scope, state } =
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


{-| The result of parsing a Navigation.Location that may have come from
a redirect from a callback server, as implemented in the `server` directory
of this project.

For a `TokenAndState` return, the state you passed is inside the `Oath.ResponseToken`.

For a `TokenErrorAndState` return, the state you passed is the `(Maybe String)`.

For the other two possibilities, your state is not available.

-}
type TokenState
    = TokenAndState OAuth.ResponseToken
    | TokenErrorAndState String (Maybe String)
    | TokenDecodeError String
    | NoToken


{-| Parse a returned `OAuth.ResponseToken` from a `Navigation.Location`
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
                    TokenAndState token

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
use : OAuth.ResponseToken -> List Http.Header -> List Http.Header
use responseToken headers =
    OAuth.use responseToken.token headers
