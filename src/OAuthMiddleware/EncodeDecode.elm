----------------------------------------------------------------------
--
-- EncodeDecode.elm
-- JSON Encoders and Decoders for OAuthMiddleware module
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module OAuthMiddleware.EncodeDecode exposing
    ( Authorization, RedirectState, ResponseTokenError
    , encodeRedirectState, decodeRedirectState
    , encodeResponseToken, decodeResponseToken
    , encodeResponseTokenError, decodeResponseTokenError
    , authorizationsDecoder, authorizationsEncoder
    , authorizationDecoder, authorizationEncoder
    , redirectStateDecoder, redirectStateEncoder
    , responseTokenDecoder, responseTokenEncoder
    , responseTokenErrorDecoder, responseTokenErrorEncoder
    , nullableStringEncoder
    )

{-| JSON Encoders and Decoders for the `OAuthMiddleware` module.


# Types

@docs Authorization, RedirectState, ResponseTokenError


# Encode/Decode state for passing over the wire.

@docs encodeRedirectState, decodeRedirectState
@docs encodeResponseToken, decodeResponseToken
@docs encodeResponseTokenError, decodeResponseTokenError


# Encoders and Decoders

@docs authorizationsDecoder, authorizationsEncoder
@docs authorizationDecoder, authorizationEncoder
@docs redirectStateDecoder, redirectStateEncoder
@docs responseTokenDecoder, responseTokenEncoder
@docs responseTokenErrorDecoder, responseTokenErrorEncoder
@docs nullableStringEncoder

-}

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import OAuth
import OAuthMiddleware.ResponseToken as OD


{-| The state sent to the `redirectUri`.

This is created by `OAuthMiddleware.authorize` from the passed `TokenAuthorization`.

-}
type alias RedirectState =
    { clientId : String
    , tokenUri : String
    , redirectUri : String
    , scope : List String
    , redirectBackUri : String
    , state : Maybe String
    }


decodeString : Decoder a -> String -> Result String a
decodeString decoder string =
    case JD.decodeString decoder string of
        Ok a ->
            Ok a

        Err error ->
            Err <| JD.errorToString error


{-| Decode the state encoded by `encodeRedirectState`.
-}
decodeRedirectState : String -> Result String RedirectState
decodeRedirectState json =
    decodeString redirectStateDecoder json


{-| Encode the redirectBackUri and user state for the authorization server.
-}
encodeRedirectState : RedirectState -> String
encodeRedirectState redirectState =
    JE.encode 0 <|
        redirectStateEncoder redirectState


{-| Decode the `ResponseToken` that is sent back to the `redirectUri`
from the redirect server.
-}
decodeResponseToken : String -> Result String OD.ResponseToken
decodeResponseToken json =
    decodeString responseTokenDecoder json


{-| Encode the `ResponseToken` that is received by the redirect server
from its call to `OAuth.AuthorizationCode.authenticate`.
-}
encodeResponseToken : OD.ResponseToken -> String
encodeResponseToken responseToken =
    JE.encode 0 <|
        responseTokenEncoder responseToken


{-| If an error occurs getting a token from the token server,
it is encoded in a `ResponseTokenError`.
-}
type alias ResponseTokenError =
    { err : String
    , state : Maybe String
    }


{-| Decode the `ResponseTokenError` that may be sent back to the
`redirectUri` from the redirect server.
-}
decodeResponseTokenError : String -> Result String ResponseTokenError
decodeResponseTokenError json =
    decodeString responseTokenErrorDecoder json


{-| Encode the `ResponseTokenError` that may be sent back to the
`redirectUri` from the redirect server.
-}
encodeResponseTokenError : ResponseTokenError -> String
encodeResponseTokenError responseTokenError =
    JE.encode 0 <|
        responseTokenErrorEncoder responseTokenError


{-| Decode the state sent to the authenticate server
via `OAuth.AuthorizationCode.authenticate` and received
by the redirect server.
-}
redirectStateDecoder : Decoder RedirectState
redirectStateDecoder =
    JD.map6 RedirectState
        (JD.field "clientId" JD.string)
        (JD.field "tokenUri" JD.string)
        (JD.field "redirectUri" JD.string)
        (JD.field "scope" <| JD.list JD.string)
        (JD.field "redirectBackUri" JD.string)
        (JD.field "state" <| JD.nullable JD.string)


{-| Simple encoder for a nullable string.
-}
nullableStringEncoder : Maybe String -> Value
nullableStringEncoder string =
    case string of
        Nothing ->
            JE.null

        Just s ->
            JE.string s


{-| Encode the state sent to the authenticate server
via `OAuth.AuthorizationCode.authenticate` and received
by the redirect server.
-}
redirectStateEncoder : RedirectState -> Value
redirectStateEncoder state =
    JE.object
        [ ( "clientId", JE.string state.clientId )
        , ( "tokenUri", JE.string state.tokenUri )
        , ( "redirectUri", JE.string state.redirectUri )
        , ( "scope", JE.list JE.string state.scope )
        , ( "redirectBackUri", JE.string state.redirectBackUri )
        , ( "state", nullableStringEncoder state.state )
        ]


tokenEncoderFields : String -> OAuth.Token -> List ( String, Value )
tokenEncoderFields field token =
    let
        s =
            OAuth.tokenToString token |> String.dropLeft 7
    in
    [ ( field
      , JE.string s
      )
    , ( "token_type", JE.string "Bearer" )
    ]


{-| Turn a `ResponseTokenError` into a `Value`.
-}
responseTokenErrorEncoder : ResponseTokenError -> Value
responseTokenErrorEncoder error =
    JE.object
        [ ( "err", JE.string error.err )
        , ( "state", nullableStringEncoder error.state )
        ]


{-| Decode a `ResponseTokenError`.
-}
responseTokenErrorDecoder : Decoder ResponseTokenError
responseTokenErrorDecoder =
    JD.map2 ResponseTokenError
        (JD.field "err" JD.string)
        (JD.field "state" <| JD.nullable JD.string)


{-| Encode the "response-token" query arg for the redirectBackUri
-}
responseTokenEncoder : OD.ResponseToken -> Value
responseTokenEncoder responseToken =
    List.concat
        [ tokenEncoderFields "access_token" responseToken.token
        , case responseToken.expiresIn of
            Nothing ->
                []

            Just ex ->
                [ ( "expires_in", JE.int ex ) ]
        , case responseToken.refreshToken of
            Nothing ->
                []

            Just token ->
                tokenEncoderFields "refresh_token" token
        , case responseToken.scope of
            [] ->
                []

            scope ->
                [ ( "scope", JE.list JE.string scope ) ]
        , case responseToken.state of
            Nothing ->
                []

            Just state ->
                [ ( "state", JE.string state ) ]
        ]
        |> JE.object



---
--- From the truqu/elm-oauth2 Internal module.
--- Not exported, so I had to copy it.
---


{-| Decode the "response-token" query arg for the redirectBackUri.

Changes the default by swapping `lenientScopeDecoder` for `scopeDecoder`.

This isn't necessary on the client side, but is needed by the server for GitHub.

-}
responseTokenDecoder : Decoder OD.ResponseToken
responseTokenDecoder =
    OD.responseTokenDecoder


{-| Authorization information to send to the redirect (callback) server.

Usually stored in a JSON file, and read and decoded by `OAuthMiddleware.getAuthorizations` or `OAuthMiddleware.getAuthorization`.

-}
type alias Authorization =
    { name : String
    , authorizationUri : String
    , tokenUri : String
    , apiUri : String
    , clientId : String
    , redirectUri : String
    , scopes : Dict String String
    }


emptyAuthorization : Authorization
emptyAuthorization =
    { name = ""
    , authorizationUri = ""
    , tokenUri = ""
    , apiUri = ""
    , clientId = ""
    , redirectUri = ""
    , scopes = Dict.empty
    }


{-| Decode an `Authorization`.

Usually used via `OAuthMiddleware.getAuthorization`.

-}
authorizationDecoder : Decoder Authorization
authorizationDecoder =
    JD.map7 Authorization
        (JD.field "name" JD.string)
        (JD.field "authorizationUri" JD.string)
        (JD.field "tokenUri" JD.string)
        (JD.field "apiUri" JD.string)
        (JD.field "clientId" JD.string)
        (JD.field "redirectUri" JD.string)
        (JD.field "scopes" <| JD.dict JD.string)


{-| Encode an `Authorization`.
-}
authorizationEncoder : Authorization -> Value
authorizationEncoder authorization =
    JE.object
        [ ( "name", JE.string authorization.name )
        , ( "authorizationUri", JE.string authorization.authorizationUri )
        , ( "tokenUri", JE.string authorization.tokenUri )
        , ( "apiUri", JE.string authorization.apiUri )
        , ( "clientId", JE.string authorization.clientId )
        , ( "redirectUri", JE.string authorization.redirectUri )
        , ( "scopes"
          , JE.object <|
                List.map (\( k, v ) -> ( k, JE.string v ))
                    (Dict.toList authorization.scopes)
          )
        ]


{-| Decode an `Authorization` list.

Usually used via `OAuthMiddleware.getAuthorizations`.

-}
authorizationsDecoder : Decoder (List Authorization)
authorizationsDecoder =
    JD.list
        (JD.oneOf
            [ JD.field "comment" JD.value
                |> JD.andThen
                    (\_ -> JD.succeed emptyAuthorization)
            , authorizationDecoder
            ]
        )
        |> JD.map (\l -> List.filter ((/=) emptyAuthorization) l)


{-| Encode an `Authorization` list.
-}
authorizationsEncoder : List Authorization -> Value
authorizationsEncoder authorizations =
    JE.list authorizationEncoder authorizations
