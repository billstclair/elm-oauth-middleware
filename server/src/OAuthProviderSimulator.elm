---------------------------------------------------------------------
--
-- OAuthProviderSimulator.elm
-- Code to simulate an OAuth Provider
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module OAuthProviderSimulator exposing
    ( handleAuthorization
    , parseAuthorization
    , providerTokenRequest
    )

{-| This is a simulator for an OAuth Provider.

It is called by OAuthTokenServer.elm, when it gets a request that looks like it's for the provider, not the redirectUri server.

-}

import Base64
import Debug
import Dict exposing (Dict)
import Http exposing (Error(..))
import Json.Encode as JE exposing (Value)
import Server.Http
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((<?>))
import Url.Parser.Query as Query


{-| If you send something that looks like an authorization request,

we behave like an authorization server that fails if the `client_id` is `fail`, or approves otherwise.

For testing.

-}
handleAuthorization : AuthorizationRequest -> Server.Http.Request -> model -> ( model, Cmd msg )
handleAuthorization authorization request model =
    let
        { clientId, redirectUri, state } =
            authorization

        query =
            Builder.relative []
                [ Builder.string "code" "xyzzy"
                , Builder.string "state" state
                ]

        location =
            redirectUri ++ query

        response =
            Server.Http.emptyResponse
                Server.Http.foundStatus
                request.id
                |> Server.Http.addHeader "location" location
    in
    ( model
    , Server.Http.send response
    )


type alias AuthorizationRequest =
    { clientId : String
    , redirectUri : String
    , state : String
    }


type alias MaybeAuthorizationRequest =
    { clientId : Maybe String
    , redirectUri : Maybe String
    , state : Maybe String
    }


parseAuthorization : Url -> Maybe AuthorizationRequest
parseAuthorization url =
    case Parser.parse authorizationParser { url | path = "" } of
        Just { clientId, redirectUri, state } ->
            case ( clientId, redirectUri, state ) of
                ( Just cid, Just ruri, Just s ) ->
                    Just <| AuthorizationRequest cid ruri s

                _ ->
                    Nothing

        _ ->
            Nothing


authorizationParser =
    Parser.top
        <?> Query.map3 MaybeAuthorizationRequest
                (Query.string "client_id")
                (Query.string "redirect_uri")
                (Query.string "state")


httpLocalhost : String
httpLocalhost =
    "http://localhost"


{-| If we get a POST, try to parse it as an authentication request.

Succeed, unless the clientId is "fail".

<https://www.oauth.com/oauth2-servers/access-tokens/authorization-code-request>

-}
providerTokenRequest : Server.Http.Request -> model -> ( model, Cmd msg )
providerTokenRequest request model =
    let
        ( uri, query ) =
            case request.body of
                Just q ->
                    ( httpLocalhost ++ "/?" ++ q, q )

                Nothing ->
                    ( httpLocalhost ++ request.url, request.url )
    in
    case Url.fromString uri of
        Nothing ->
            ( model
            , invalidTokenRequest
                ("Can't parse provider token request: " ++ query)
                request
            )

        Just url ->
            case parseProviderTokenRequest url of
                Nothing ->
                    ( model
                    , invalidTokenRequest
                        ("Malformed provider token request: " ++ query)
                        request
                    )

                Just { clientId, clientSecret } ->
                    case ( clientId, clientSecret ) of
                        ( Just cid, Just _ ) ->
                            if cid == "fail" then
                                ( model, invalidTokenClient request )

                            else
                                ( model, providerTokenResponse request )

                        _ ->
                            case checkProviderTokenAuthorization request of
                                Ok ( cid, _ ) ->
                                    if cid == "fail" then
                                        ( model, invalidTokenClient request )

                                    else
                                        ( model, providerTokenResponse request )

                                Err err ->
                                    ( model, invalidTokenRequest err request )


tokenErrorJson : String -> String -> String
tokenErrorJson error description =
    let
        value =
            JE.object
                [ ( "error", JE.string error )
                , ( "error_description", JE.string description )
                , ( "error_uri"
                  , JE.string "See https://www.oauth.com/oauth2-servers/access-tokens/authorization-code-request"
                  )
                ]
    in
    JE.encode 0 value


tokenErrorCmd : Server.Http.Status -> String -> Server.Http.Request -> Cmd msg
tokenErrorCmd status body request =
    let
        response =
            Server.Http.textResponse status body request.id
                |> Server.Http.addHeader "Cache-Control" "no-store"
                |> Server.Http.addHeader "Pragma" "no-cache"
    in
    Server.Http.send response


{-| <https://www.oauth.com/oauth2-servers/access-tokens/access-token-response>
-}
invalidTokenRequest : String -> Server.Http.Request -> Cmd msg
invalidTokenRequest err request =
    let
        json =
            tokenErrorJson "invalid_request" err
    in
    tokenErrorCmd Server.Http.badRequestStatus json request


{-| <https://www.oauth.com/oauth2-servers/access-tokens/access-token-response>
-}
invalidTokenClient : Server.Http.Request -> Cmd msg
invalidTokenClient request =
    let
        json =
            tokenErrorJson "invalid_client" "Client authentication failed."
    in
    tokenErrorCmd Server.Http.unauthorizedStatus json request


tokenResponseJson : Int -> String
tokenResponseJson expiresIn =
    let
        value =
            JE.object
                [ ( "access_token", JE.string "yourTokenSir" )
                , ( "token_type", JE.string "bearer" )
                , ( "expires_in", JE.int expiresIn )
                , ( "refresh_token", JE.string "aRefreshToken" )
                ]
    in
    JE.encode 0 value


{-| <https://www.oauth.com/oauth2-servers/access-tokens/access-token-response>
-}
providerTokenResponse : Server.Http.Request -> Cmd msg
providerTokenResponse request =
    let
        json =
            tokenResponseJson 3600

        response =
            Server.Http.textResponse Server.Http.okStatus json request.id
    in
    Server.Http.send response


checkProviderTokenAuthorization : Server.Http.Request -> Result String ( String, String )
checkProviderTokenAuthorization request =
    case Dict.get "authorization" request.headers of
        Nothing ->
            Err "Missing authorization"

        Just authorization ->
            let
                basic =
                    String.left 6 authorization

                base64 =
                    String.dropLeft 6 authorization
            in
            if basic /= "Basic " then
                Err ("Unsupported authorization: " ++ authorization)

            else
                case Base64.decode base64 of
                    Err _ ->
                        Err ("Can't decode authorization: " ++ authorization)

                    Ok idAndSecret ->
                        case String.split ":" idAndSecret of
                            [ id, secret ] ->
                                Ok ( id, secret )

                            _ ->
                                Err
                                    ("Basic authorization not 'id:secret': "
                                        ++ idAndSecret
                                    )


type alias ProviderTokenRequest =
    { clientId : Maybe String
    , clientSecret : Maybe String
    }


type alias MaybeProviderTokenRequest =
    { grantType : Maybe String
    , code : Maybe String
    , clientId : Maybe String
    , clientSecret : Maybe String
    }


authorizationCodeGrantType : String
authorizationCodeGrantType =
    "authorization_code"


parseProviderTokenRequest : Url -> Maybe ProviderTokenRequest
parseProviderTokenRequest url =
    case Parser.parse providerTokenParser { url | path = "" } of
        Just { grantType, code, clientId, clientSecret } ->
            case ( grantType, code ) of
                ( Just gt, Just _ ) ->
                    if gt == authorizationCodeGrantType then
                        Just <| ProviderTokenRequest clientId clientSecret

                    else
                        Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


providerTokenParser =
    Parser.top
        <?> Query.map4 MaybeProviderTokenRequest
                (Query.string "grant_type")
                (Query.string "code")
                (Query.string "client_id")
                (Query.string "client_secret")
