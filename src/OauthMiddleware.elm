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

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import OAuth
import OAuth.AuthorizationCode


{-| Configuration for sending a request to the authorization server.
-}
type alias Authorization =
    { clientId : String
    , redirectUri : String
    , redirectBackUri : String
    , scope : List String
    , state : Maybe String
    , url : String
    }


{-| Send an authorization request.

This will cause the authorization server to ask the user to login. If successful, it will send the received code and `Authorization.state` to the `Authorization.redirectUri` for generation of a token to send back to the `Authorization.redirectBackUri`. Your code at that Uri will pass the encoded token and state to `receiveToken`, for unpackaging.

-}
authorize : Authorization -> Cmd msg
authorize { clientId, redirectUri, redirectBackUri, scope, state, url } =
    OAuth.AuthorizationCode.authorize
        { clientId = clientId
        , redirectUri = redirectUri
        , responseType = OAuth.Code
        , scope = scope
        , state = Just <| encodeRedirectState redirectBackUri state
        , url = url
        }


{-| Decode the state sent to the authorization server.

When your code is invoked at the `Authorization.redirectBackUri` passed to `authorize`, you call `receiveToken` with the `state` you receive in the URL, and this function decodes it into an authorization token and the `Authorization.state` you passed to `authorize`.

The server uses `encodeToken` to create this string.

-}
decodeToken : String -> Result String ( String, Maybe String )
decodeToken json =
    case JD.decodeString tokenStateDecoder json of
        Err msg ->
            Err msg

        Ok { token, state } ->
            Ok ( token, state )


{-| Encode the token and user state for the redirectBackUri.
-}
encodeToken : String -> Maybe String -> String
encodeToken token state =
    JE.encode 0 <|
        tokenStateEncoder
            { token = token, state = state }


{-| Decode the state encoded by `encodeRedirectState`.
-}
decodeRedirectState : String -> Result String ( String, Maybe String )
decodeRedirectState json =
    case JD.decodeString redirectStateDecoder json of
        Err msg ->
            Err msg

        Ok { redirectBackUri, state } ->
            Ok ( redirectBackUri, state )


{-| Encode the redirectBackUri and user state for the authorization server.
-}
encodeRedirectState : String -> Maybe String -> String
encodeRedirectState redirectBackUri state =
    JE.encode 0 <|
        redirectStateEncoder
            { redirectBackUri = redirectBackUri, state = state }



---
--- Internals
---


type alias RedirectState =
    { redirectBackUri : String
    , state : Maybe String
    }


type alias TokenState =
    { token : String
    , state : Maybe String
    }


redirectStateDecoder : Decoder RedirectState
redirectStateDecoder =
    JD.map2 RedirectState
        (JD.field "redirectBackUri" <| JD.string)
        (JD.field "state" <| JD.nullable JD.string)


redirectStateEncoder : RedirectState -> Value
redirectStateEncoder state =
    JE.object
        [ ( "redirectBackUri", JE.string state.redirectBackUri )
        , ( "state"
          , case state.state of
                Nothing ->
                    JE.null

                Just s ->
                    JE.string s
          )
        ]


tokenStateDecoder : Decoder TokenState
tokenStateDecoder =
    JD.map2 TokenState
        (JD.field "token" <| JD.string)
        (JD.field "state" <| JD.nullable JD.string)


tokenStateEncoder : TokenState -> Value
tokenStateEncoder state =
    JE.object
        [ ( "token", JE.string state.token )
        , ( "state"
          , case state.state of
                Nothing ->
                    JE.null

                Just s ->
                    JE.string s
          )
        ]
