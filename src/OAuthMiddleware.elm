----------------------------------------------------------------------
--
-- OAuthMiddleware.elm
-- Client side of an OAuth Authorization Code Grant Flow
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module OAuthMiddleware exposing
    ( Authorization, TokenAuthorization, ResponseToken, TokenState(..)
    , getAuthorization, getAuthorizations
    , locationToRedirectBackUri, authorize, receiveTokenAndState, use
    )

{-| Client side of OAuth Authorization Code Grant Flow.

Your top-level Elm program must be created with `Navigation.program` or `Navigation.programWithFlags`. You can ignore incoming `Location` changes, but your `init` function needs to pass the initial `Location` to `receiveTokenAndState`, in case you're here because of a redirect back from your OAuth callback server (at `redirectUri`).


# Types

@docs Authorization, TokenAuthorization, ResponseToken, TokenState


# Client-side functions

@docs getAuthorization, getAuthorizations
@docs locationToRedirectBackUri, authorize, receiveTokenAndState, use

-}

import Base64
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import OAuth
import OAuth.AuthorizationCode as AuthorizationCode exposing (RequestParts)
import OAuthMiddleware.EncodeDecode as ED
import OAuthMiddleware.ResponseToken as ResponseToken
import Task exposing (Task)
import Url exposing (Url)


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


getNoCache : (Result Http.Error a -> msg) -> Bool -> String -> Decoder a -> RequestParts msg
getNoCache tagger useCache url decoder =
    { method = "GET"
    , headers =
        if useCache then
            []

        else
            [ Http.header
                "Cache-Control"
                "no-cache, no-store, must-revalidate"
            ]
    , url = url
    , body = Http.emptyBody
    , expect = Http.expectJson tagger decoder
    , timeout = Nothing
    , tracker = Nothing
    }


{-| Get a JSON file encoding an `Authorization`.

    getAuthorization useCache url

If `useCache` is true, will use the browser's cache, meaning that it may not immediately notice changes to the file on the server.

The JSON format is as follows. You'll change it with information from your OAuth provider, and for your redirect server, and store it in a convenient place on the same server serving your compiled Elm code.

The `scopes` field is an object, mapping your internal name for each scope to the actual OAuth provider name. For most OAuth providers, the two will be identical, but Google, for example, uses long URL-looking strings for scope names, so it's convenient to have a shorter name your application can use. This field isn't used by any of the `OAuthMiddleware` code, except the example, so you can safely set it to `{}`, if you prefer to just encode the scope strings as constants in your Elm code.

Your client secret is stored with the redirect server, and never leaves that server machine.

    { "name": "Gmail",
      "authorizationUri": "https://accounts.google.com/o/oauth2/auth",
      "tokenUri": "https://accounts.google.com/o/oauth2/token",
      "apiUri": "https://www.googleapis.com/gmail/v1/users/",
      "clientId": "<Your OAuth clientid>",
      "redirectUri": "<Your redirect server Uri>",
      "scopes": {"<Your scope name>": "<OAuth provider's scope name>"}
    }

The example contains more information about this, and a sample authorizations file.

-}
getAuthorization : (Result Http.Error Authorization -> msg) -> Bool -> String -> RequestParts msg
getAuthorization tagger useCache url =
    getNoCache tagger useCache url ED.authorizationDecoder


{-| Get a JSON file encoding an `Authorization` list.

    getAuthorizations useCache url

If `useCache` is true, will use the browser's cache, meaning that it may not immediately notice changes to the file on the server.

-}
getAuthorizations : (Result Http.Error (List Authorization) -> msg) -> Bool -> String -> RequestParts msg
getAuthorizations tagger useCache url =
    getNoCache tagger useCache url ED.authorizationsDecoder


protocolString : Url.Protocol -> String
protocolString protocol =
    case protocol of
        Url.Http ->
            "http:"

        Url.Https ->
            "https:"


{-| Convert a `Url.Url` into a string suitable for the `redirectBackUri` in a `TokenAuthorization`.
-}
locationToRedirectBackUri : Url -> String
locationToRedirectBackUri url =
    protocolString url.protocol
        ++ "//"
        ++ url.host
        ++ (case url.port_ of
                Nothing ->
                    ""

                Just p ->
                    ":" ++ String.fromInt p
           )
        ++ url.path


{-| Compute an authorization request Url.

This will cause the authorization server to ask the user to login. If successful, it will send the received code and `TokenAuthorization.state` to the `TokenAuthorization.authorization.redirectUri` for generation of a token to send back to the `TokenAuthorization.redirectBackUri`. Your code at that Uri will receive an encoded `ResponseToken` on the `responseTokenQuery` parameter, or an error string on the `responseTokenQueryError` parameter. Use `decodeResponseToken` to turn the `responseTokenQuery` string into an `ResponseToken`, which you can use to do authenticated requests, just as if you had called `OAuth.TokenAuthorizationCode.authenticate` yourself, but hiding the client secret on the redirect server.

A return value of `Nothing` means that either the authorization uri or the redirect uri could not be parsed as valid uris.

Otherwise, navigating to the the returned `Url` will go to the the OAuth provider's authentication page, followed by token fetching by the redirect server at `redirectUri`. The redirect server will navigate back with query args that you can process with `receiveTokenAndState`.

-}
authorize : TokenAuthorization -> Maybe Url
authorize { authorization, redirectBackUri, scope, state } =
    let
        { authorizationUri, tokenUri, clientId, redirectUri } =
            authorization

        wireState =
            ED.encodeRedirectState
                { clientId = clientId
                , tokenUri = tokenUri
                , redirectUri = redirectUri
                , scope = scope
                , redirectBackUri = redirectBackUri
                , state = state
                }

        base64 =
            Base64.encode wireState
    in
    case ( Url.fromString authorizationUri, Url.fromString redirectUri ) of
        ( Just authorizationUrl, Just redirectUrl ) ->
            Just <|
                AuthorizationCode.makeAuthUrl
                    { clientId = clientId
                    , url = authorizationUrl
                    , redirectUri = redirectUrl
                    , scope = scope
                    , state = Just base64
                    }

        _ ->
            Nothing


{-| An alias for `OAuth.ResponseToken`.
-}
type alias ResponseToken =
    ResponseToken.ResponseToken


{-| The result of parsing a Url.Url that may have come from
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


{-| Parse a returned `ResponseToken` from a `Url.Url`.

Note that the `scope` in the returned `TokenAndState` `ResponseToken` is not guaranteed to match what you requested. The RFC 6749 [Authorization Code Grant](https://tools.ietf.org/html/rfc6749#section-4.1) flow does not specify a returned scope from the OAuth token server, so if your redirectUri server receives no scope, or an empty scope, it will send back the list of scopes you requested. The spec does not guarantee that you'll get all the scopes you requested in your call to `authorize`, so this may be incorrect. It appears that GitHub returns proper scopes, so they will be as granted in that case.

-}
receiveTokenAndState : Url -> TokenState
receiveTokenAndState url =
    case url.fragment of
        Nothing ->
            NoToken

        Just base64 ->
            case Base64.decode base64 of
                Err _ ->
                    NoToken

                Ok reply ->
                    case ED.decodeResponseToken reply of
                        Ok token ->
                            TokenAndState token token.state

                        Err _ ->
                            case ED.decodeResponseTokenError reply of
                                Ok error ->
                                    TokenErrorAndState error.err error.state

                                Err _ ->
                                    NoToken


{-| Use a token to add authenticatication to a request header.

A thin wrapper around `OAuth.useToken`.

-}
use : ResponseToken -> List Http.Header -> List Http.Header
use responseToken =
    OAuth.useToken responseToken.token
