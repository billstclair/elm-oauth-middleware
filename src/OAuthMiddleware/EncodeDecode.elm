----------------------------------------------------------------------
--
-- EncodeDecode.elm
-- JSON Encoders and Decoders for OAuthMiddleware module
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module OAuthMiddleware.EncodeDecode
    exposing
        ( Authorization
        , RedirectState
        , ResponseTokenError
        , authorizationDecoder
        , authorizationEncoder
        , authorizationsDecoder
        , authorizationsEncoder
        , decodeRedirectState
        , decodeResponseToken
        , decodeResponseTokenError
        , encodeRedirectState
        , encodeResponseToken
        , encodeResponseTokenError
        , nullableStringEncoder
        , redirectStateDecoder
        , redirectStateEncoder
        , responseTokenDecoder
        , responseTokenEncoder
        , responseTokenErrorDecoder
        , responseTokenErrorEncoder
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


{-| The state sent to the `redirectUri`.
-}
type alias RedirectState =
    { clientId : String
    , tokenUri : String
    , redirectUri : String
    , scope : List String
    , redirectBackUri : String
    , state : Maybe String
    }


{-| Decode the state encoded by `encodeRedirectState`.
-}
decodeRedirectState : String -> Result String RedirectState
decodeRedirectState json =
    JD.decodeString redirectStateDecoder json


{-| Encode the redirectBackUri and user state for the authorization server.
-}
encodeRedirectState : RedirectState -> String
encodeRedirectState redirectState =
    JE.encode 0 <|
        redirectStateEncoder redirectState


{-| Decode the `ResponseToken` that is sent back to the `redirectUri`
from the redirect server.
-}
decodeResponseToken : String -> Result String OAuth.ResponseToken
decodeResponseToken json =
    JD.decodeString responseTokenDecoder json


{-| Encode the `ResponseToken` that is received by the redirect server
from its call to `OAuth.AuthorizationCode.authenticate`.
-}
encodeResponseToken : OAuth.ResponseToken -> String
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
    JD.decodeString responseTokenErrorDecoder json


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
        , ( "scope", JE.list <| List.map JE.string state.scope )
        , ( "redirectBackUri", JE.string state.redirectBackUri )
        , ( "state", nullableStringEncoder state.state )
        ]


tokenEncoderFields : String -> OAuth.Token -> List ( String, Value )
tokenEncoderFields field token =
    [ ( field
      , case token of
            OAuth.Bearer s ->
                JE.string s
      )
    , ( "token_type", JE.string "bearer" )
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
responseTokenEncoder : OAuth.ResponseToken -> Value
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
                [ ( "scope", JE.list <| List.map JE.string scope ) ]
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


{-| Decode the "response-token" query arg for the redirectBackUri
-}
responseTokenDecoder : Decoder OAuth.ResponseToken
responseTokenDecoder =
    JD.oneOf
        [ JD.map5
            (\token expiresIn refreshToken scope state ->
                { token = token
                , expiresIn = expiresIn
                , refreshToken = refreshToken
                , scope = Maybe.withDefault [] scope
                , state = state
                }
            )
            accessTokenDecoder
            (JD.maybe <| JD.field "expires_in" JD.int)
            refreshTokenDecoder
            (JD.maybe <| JD.field "scope" (JD.list JD.string))
            (JD.maybe <| JD.field "state" JD.string)
        ]


accessTokenDecoder : JD.Decoder OAuth.Token
accessTokenDecoder =
    let
        mtoken =
            JD.map2 makeToken
                (JD.field "access_token" JD.string |> JD.map Just)
                (JD.field "token_type" JD.string)

        failUnless =
            Maybe.map JD.succeed >> Maybe.withDefault (JD.fail "can't decode token")
    in
    JD.andThen failUnless mtoken


refreshTokenDecoder : JD.Decoder (Maybe OAuth.Token)
refreshTokenDecoder =
    JD.map2 makeToken
        (JD.maybe <| JD.field "refresh_token" JD.string)
        (JD.field "token_type" JD.string)


makeToken : Maybe String -> String -> Maybe OAuth.Token
makeToken mtoken tokenType =
    case ( mtoken, String.toLower tokenType ) of
        ( Just token, "bearer" ) ->
            Just <| OAuth.Bearer token

        _ ->
            Nothing


{-| Authorization information to send to the redirect (callback) server.

Usually stored in a JSON file, and read and decoded by `OAuthMiddleware.getAuthorizations` or `OAuthMiddleware.getAuthorization`.

-}
type alias Authorization =
    { name : String
    , authorizationUri : String
    , tokenUri : String
    , clientId : String
    , redirectUri : String
    , scopes : Dict String String
    }


{-| Decode an `Authorization`.

Usually used via `OAuthMiddleware.getAuthorization`.

-}
authorizationDecoder : Decoder Authorization
authorizationDecoder =
    JD.map6 Authorization
        (JD.field "name" JD.string)
        (JD.field "authorizationUri" JD.string)
        (JD.field "tokenUri" JD.string)
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
    JD.list authorizationDecoder


{-| Encode an `Authorization` list.
-}
authorizationsEncoder : List Authorization -> Value
authorizationsEncoder authorizations =
    JE.list <| List.map authorizationEncoder authorizations
