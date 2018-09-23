----------------------------------------------------------------------
--
-- ResponseToken.elm
-- The ResponseToken type and decoders for it.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module OAuthMiddleware.ResponseToken exposing
    ( ResponseToken
    , responseDecoder, expiresInDecoder, scopeDecoder, lenientScopeDecoder, stateDecoder, accessTokenDecoder, refreshTokenDecoder
    , makeToken, makeResponseToken
    )

{-| This module revives the `ResponseToken` type.

`ResponseToken` and its decoders were removed from truqu/elm-oauth2 version 4.0.0. This module is a copy of the version 3.0.0 code.


## Types

@docs ResponseToken


## Json Decoders

@docs responseDecoder, expiresInDecoder, scopeDecoder, lenientScopeDecoder, stateDecoder, accessTokenDecoder, refreshTokenDecoder


## Constructors

@docs makeToken, makeResponseToken

-}

import Json.Decode as Json
import OAuth exposing (..)


{-| The response obtained as a result of an authentication (implicit or not)

  - expiresIn (_RECOMMENDED_):
    The lifetime in seconds of the access token. For example, the value "3600" denotes that the
    access token will expire in one hour from the time the response was generated. If omitted, the
    authorization server SHOULD provide the expiration time via other means or document the default
    value.
  - refreshToken (_OPTIONAL_):
    The refresh token, which can be used to obtain new access tokens using the same authorization
    grant as described in [Section 6](https://tools.ietf.org/html/rfc6749#section-6).
  - scope (_OPTIONAL, if identical to the scope requested; otherwise, REQUIRED_):
    The scope of the access token as described by [Section 3.3](https://tools.ietf.org/html/rfc6749#section-3.3).
  - state (_REQUIRED if `state` was present in the authentication request_):
    The exact value received from the client
  - token (_REQUIRED_):
    The access token issued by the authorization server.

-}
type alias ResponseToken =
    { expiresIn : Maybe Int
    , refreshToken : Maybe Token
    , scope : List String
    , state : Maybe String
    , token : Token
    }


{-| Json decoder for a response. You may provide a custom response decoder using other decoders
from this module, or some of your own craft.

For instance,

    myScopeDecoder : Json.Decoder (Maybe (List String))
    myScopeDecoder =
        Json.maybe <|
            Json.oneOf
                [ Json.field "scope" (Json.map (String.split ",") Json.string) ]

    myResponseDecoder : Json.Decoder ResponseToken
    myResponseDecoder =
        Json.map5 makeResponseToken
            accessTokenDecoder
            expiresInDecoder
            refreshTokenDecoder
            myScopeDecoder
            stateDecoder

-}
responseDecoder : Json.Decoder ResponseToken
responseDecoder =
    Json.map5 makeResponseToken
        accessTokenDecoder
        expiresInDecoder
        refreshTokenDecoder
        scopeDecoder
        stateDecoder


{-| Json decoder for an expire timestamp
-}
expiresInDecoder : Json.Decoder (Maybe Int)
expiresInDecoder =
    Json.maybe <| Json.field "expires_in" Json.int


{-| Json decoder for a scope
-}
scopeDecoder : Json.Decoder (Maybe (List String))
scopeDecoder =
    Json.maybe <| Json.field "scope" (Json.list Json.string)


{-| Json decoder for a scope, allowing comma- or space-separated scopes
-}
lenientScopeDecoder : Json.Decoder (Maybe (List String))
lenientScopeDecoder =
    Json.maybe <|
        Json.field "scope" <|
            Json.oneOf
                [ Json.list Json.string
                , Json.map (String.split ",") Json.string
                ]


{-| Json decoder for a state
-}
stateDecoder : Json.Decoder (Maybe String)
stateDecoder =
    Json.maybe <| Json.field "state" Json.string


{-| Json decoder for an access token
-}
accessTokenDecoder : Json.Decoder Token
accessTokenDecoder =
    let
        mtoken =
            Json.map2 makeToken
                (Json.field "access_token" Json.string |> Json.map Just)
                (Json.field "token_type" Json.string)

        failUnless =
            Maybe.map Json.succeed >> Maybe.withDefault (Json.fail "can't decode token")
    in
    Json.andThen failUnless mtoken


{-| Json decoder for a refresh token
-}
refreshTokenDecoder : Json.Decoder (Maybe Token)
refreshTokenDecoder =
    Json.map2 makeToken
        (Json.maybe <| Json.field "refresh_token" Json.string)
        (Json.field "token_type" Json.string)


{-| Create a ResponseToken record from various parameters
-}
makeResponseToken : Token -> Maybe Int -> Maybe Token -> Maybe (List String) -> Maybe String -> ResponseToken
makeResponseToken token expiresIn refreshToken scope state =
    { token = token
    , expiresIn = expiresIn
    , refreshToken = refreshToken
    , scope = Maybe.withDefault [] scope
    , state = state
    }


{-| Create a Token from a value and token type. Note that only bearer token are supported
-}
makeToken : Maybe String -> String -> Maybe Token
makeToken mtoken tokenType =
    case ( mtoken, String.toLower tokenType ) of
        ( Just token, "bearer" ) ->
            tokenFromString ("Bearer " ++ token)

        _ ->
            Nothing
