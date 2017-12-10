----------------------------------------------------------------------
--
-- OauthMiddleware.elm
-- Client side of an OAuth Authorization Code Grant Flow
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module OauthMiddleware
    exposing
        ( Authorization
        , authorize
        , decodeRedirectState
        , decodeToken
        , encodeRedirectState
        , encodeToken
        )

{-| Client side of OAuth Authorization Code Grant Flow


# Types

@docs Authorization


# Client-side functions

@docs authorize, decodeToken


# Encode/Decode state for passing over the wire.

@docs encodeToken, decodeRedirectState, encodeRedirectState

-}


{-| Configuration for sending a request to the authorization server.
-}
type alias Authorization =
    { url : String
    , clientId : String
    , redirectURi : String
    , redirectBackUri : String
    , scope : List String
    , state : Maybe String
    }


{-| Send an authorization request.

This will cause the authorization server to ask the user to login. If successful, it will send the received code and `Authorization.state` to the `Authorization.redirectUri` for generation of a token to send back to the `Authorization.redirectBackUri`. Your code at that Uri will pass the encoded token and state to `receiveToken`, for unpackaging.

-}
authorize : Authorization -> Cmd msg
authorize authorization =
    Cmd.none


{-| Decode the state sent to the authorization server.

When your code is invoked at the `Authorization.redirectBackUri` passed to `authorize`, you call `receiveToken` with the `state` you receive in the URL, and this function decodes it into an authorization token and the `Authorization.state` you passed to `authorize`.

The server uses `encodeToken` to create this string.

-}
decodeToken : String -> Result String ( String, Maybe String )
decodeToken encodedState =
    Err "Not implemented."


{-| Encode the token and user state for the redirectBackUri.
-}
encodeToken : String -> Maybe String -> String
encodeToken token state =
    "TODO"


{-| Decode the state encoded by `encodeRedirectState`.
-}
decodeRedirectState : String -> Result String ( String, Maybe String )
decodeRedirectState json =
    Err "Not implemented"


{-| Encode the redirectBackUri and user state for the authorization server.
-}
encodeRedirectState : String -> Maybe String -> String
encodeRedirectState redirectBackUri state =
    "TODO"
