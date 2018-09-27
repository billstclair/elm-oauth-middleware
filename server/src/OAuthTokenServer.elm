---------------------------------------------------------------------
--
-- OAuthTokenServer.elm
-- Top-level file for OAuthMiddleware redirectBackUri server.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


port module OAuthTokenServer exposing (main)

import Base64
import Browser
import Debug
import Dict exposing (Dict)
import Http exposing (Error(..))
import Json.Decode as JD
import Json.Encode as JE
import List.Extra as LE
import OAuth.AuthorizationCode as AC exposing (AuthenticationSuccess, RequestParts)
import OAuthMiddleware.EncodeDecode as ED exposing (RedirectState)
import OAuthMiddleware.ResponseToken exposing (ResponseToken)
import OAuthMiddleware.ServerConfiguration
    exposing
        ( LocalServerConfiguration
        , RemoteServerConfiguration
        , configurationsDecoder
        , defaultLocalServerConfiguration
        )
import Server.Http
import Time exposing (Posix)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((<?>))
import Url.Parser.Query as Query


port getFile : String -> Cmd msg


port receiveFile : (Maybe String -> msg) -> Sub msg


port httpListen : Int -> Cmd msg


type alias RedirectDict =
    Dict ( String, String ) RemoteServerConfiguration


addConfigToDict : RemoteServerConfiguration -> RedirectDict -> RedirectDict
addConfigToDict config dict =
    Dict.insert ( config.clientId, config.tokenUri ) config dict


buildRedirectDict : List RemoteServerConfiguration -> RedirectDict
buildRedirectDict configs =
    List.foldl addConfigToDict Dict.empty configs


type alias Model =
    { configString : String
    , config : List RemoteServerConfiguration
    , redirectDict : RedirectDict
    , localConfig : LocalServerConfiguration
    }


type Msg
    = NoOp
    | ReceiveConfig (Maybe String)
    | ProbeConfig Posix
    | NewRequest Server.Http.Request
    | ReceiveToken Server.Http.Id String (List String) (Maybe String) (Result Http.Error AuthenticationSuccess)


init : () -> ( Model, Cmd Msg )
init () =
    ( { configString = ""
      , config = []
      , redirectDict = Dict.empty
      , localConfig = { defaultLocalServerConfiguration | httpPort = -4321 }
      }
    , getConfig
    )


configFile : String
configFile =
    "build/config.json"


getConfig : Cmd msg
getConfig =
    getFile configFile


receiveConfig : Maybe String -> Model -> ( Model, Cmd Msg )
receiveConfig file model =
    case file of
        Nothing ->
            if model.configString == "error" then
                ( model
                , Cmd.none
                )

            else
                let
                    m1 =
                        Debug.log "Notice" ("Error reading " ++ configFile)
                in
                let
                    m2 =
                        if model.config == [] then
                            Debug.log "Fatal"
                                "Empty configuration makes me a very useless server."

                        else
                            ""
                in
                ( { model | configString = "error" }
                , Cmd.none
                )

        Just json ->
            if json == model.configString then
                ( model
                , Cmd.none
                )

            else
                case JD.decodeString configurationsDecoder json of
                    Err msg ->
                        let
                            m =
                                Debug.log "Error" msg
                        in
                        let
                            m2 =
                                if model.config == [] then
                                    Debug.log "Fatal"
                                        "Empty configuration makes me a very useless server."

                                else
                                    ""
                        in
                        ( { model | configString = json }
                        , Cmd.none
                        )

                    Ok { local, remote } ->
                        let
                            m =
                                if remote == [] then
                                    Debug.log "Notice"
                                        "Empty remote configuration disabled server."

                                else
                                    Debug.log "Notice"
                                        ("Successfully parsed " ++ configFile)
                        in
                        updateLocalConfig
                            local
                            { model
                                | configString = json
                                , config = remote
                                , redirectDict = buildRedirectDict remote
                            }


updateLocalConfig : LocalServerConfiguration -> Model -> ( Model, Cmd Msg )
updateLocalConfig config model =
    let
        cmd =
            if config.httpPort == model.localConfig.httpPort then
                Cmd.none

            else
                httpListen config.httpPort
    in
    ( { model | localConfig = config }
    , cmd
    )


authenticationErrorToString : Http.Error -> String
authenticationErrorToString error =
    case error of
        Http.BadStatus { body } ->
            case JD.decodeString AC.defaultAuthenticationErrorDecoder body of
                Ok { errorDescription } ->
                    case errorDescription of
                        Just desc ->
                            desc

                        Nothing ->
                            httpErrorToString error

                _ ->
                    httpErrorToString error

        _ ->
            httpErrorToString error


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        BadUrl s ->
            "BadUrl: " ++ s

        Timeout ->
            "Timeout"

        NetworkError ->
            "NetworkError"

        BadStatus _ ->
            "BadStatus"

        BadPayload s _ ->
            "BadPayload: " ++ s


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveConfig file ->
            receiveConfig file model

        ProbeConfig _ ->
            ( model
            , getConfig
            )

        NewRequest request ->
            newRequest request model

        ReceiveToken id redirectBackUri scope state result ->
            let
                string =
                    case result of
                        Err err ->
                            ED.encodeResponseTokenError
                                { err = authenticationErrorToString err
                                , state = state
                                }

                        Ok success ->
                            ED.encodeResponseToken
                                { token = success.token
                                , refreshToken = success.refreshToken
                                , expiresIn = success.expiresIn
                                , scope =
                                    if [] == success.scope then
                                        scope

                                    else
                                        success.scope
                                , state = state
                                }

                base64 =
                    Base64.encode string

                location =
                    redirectBackUri ++ "#" ++ base64

                response =
                    Server.Http.emptyResponse
                        Server.Http.foundStatus
                        id
                        |> Server.Http.addHeader "location" location
            in
            ( model
            , Server.Http.send response
            )

        NoOp ->
            ( model
            , Cmd.none
            )


{-| The signature really isn't interesting here.
-}
codeAndStateParser =
    Parser.top
        <?> Query.map2 Tuple.pair
                (Query.string "code")
                (Query.string "state")


parseCodeAndState : Url -> Maybe ( String, String )
parseCodeAndState url =
    case Parser.parse codeAndStateParser { url | path = "" } of
        Just ( Just code, Just state ) ->
            Just ( code, state )

        _ ->
            Nothing


{-| The request comes from the authorization server, and is of the form:

...?code=<long code>&state=<from OAuthMiddleware.EncodeDecode.encodeRedirectState>

We can also get "?error=access\_denied&state=<foo>"

-}
newRequest : Server.Http.Request -> Model -> ( Model, Cmd Msg )
newRequest request model =
    case request.method of
        Server.Http.Post ->
            oauthProviderRequest request model

        _ ->
            redirectUriRequest request model


redirectUriRequest : Server.Http.Request -> Model -> ( Model, Cmd Msg )
redirectUriRequest request model =
    case Url.fromString ("http://localhost" ++ Debug.log "url" request.url) of
        Nothing ->
            -- This shouldn't happen
            ( model
            , Server.Http.send <|
                Server.Http.textResponse
                    Server.Http.badRequestStatus
                    ("Can't parse url: " ++ request.url)
                    request.id
            )

        Just url ->
            case parseCodeAndState url of
                Just ( code, state ) ->
                    authRequest code state url request model

                _ ->
                    case parseAuthorization url of
                        Just authorization ->
                            handleAuthorization authorization request model

                        Nothing ->
                            ( model
                            , Server.Http.send <|
                                Server.Http.textResponse
                                    Server.Http.badRequestStatus
                                    "Bad request, missing code/state"
                                    request.id
                            )


{-| If you send something that looks like an authorization request,

we behave like an authorization server that always approves.

For testing.

-}
handleAuthorization : AuthorizationRequest -> Server.Http.Request -> Model -> ( Model, Cmd Msg )
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


{-| If we get a POST, try to parse it as an authentication request.

Succeed, unless the clientId is "fail".

-}
oauthProviderRequest : Server.Http.Request -> Model -> ( Model, Cmd Msg )
oauthProviderRequest request model =
    let
        req =
            Debug.log "oauthProviderRequest" request
    in
    ( model, Cmd.none )


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


authorizationParser =
    Parser.top
        <?> Query.map3 MaybeAuthorizationRequest
                (Query.string "client_id")
                (Query.string "redirect_uri")
                (Query.string "state")


{-| This was needed before the update to truqu/elm-oauth2 4.0.0.

It probably still is.

-}
adjustRequest : RequestParts a -> RequestParts a
adjustRequest req =
    let
        headers =
            Http.header "Accept" "application/json" :: req.headers
    in
    { req | headers = headers }


tokenRequest : RedirectState -> String -> Model -> Result String (Http.Request AuthenticationSuccess)
tokenRequest { clientId, tokenUri, redirectUri, scope, redirectBackUri } code model =
    let
        key =
            ( clientId, tokenUri )
    in
    case Url.fromString redirectBackUri of
        Nothing ->
            Err <| "Can't parse redirectBackUri: " ++ redirectBackUri

        Just { host, protocol } ->
            case Dict.get key model.redirectDict of
                Nothing ->
                    Err <|
                        "Unknown (clientId, tokenUri): ("
                            ++ clientId
                            ++ ", "
                            ++ tokenUri
                            ++ ")"

                Just { clientSecret, redirectBackHosts } ->
                    case LE.find (\rbh -> rbh.host == host) redirectBackHosts of
                        Nothing ->
                            Err <| "Unknown redirect host: " ++ host

                        Just { ssl } ->
                            if ssl && protocol /= Url.Https then
                                Err <| "https protocol required for redirect host: " ++ host

                            else
                                case
                                    ( Url.fromString redirectUri
                                    , Url.fromString tokenUri
                                    )
                                of
                                    ( Just redirectUrl, Just tokenUrl ) ->
                                        let
                                            requestParts =
                                                AC.makeTokenRequest
                                                    { credentials =
                                                        { clientId = clientId
                                                        , secret = Just clientSecret
                                                        }
                                                    , code = code
                                                    , redirectUri = redirectUrl
                                                    , url = tokenUrl
                                                    }
                                        in
                                        Http.request
                                            (adjustRequest requestParts)
                                            |> Ok

                                    _ ->
                                        Err "Can't parse redirectUri or tokenUri"


authRequest : String -> String -> Url -> Server.Http.Request -> Model -> ( Model, Cmd Msg )
authRequest code b64State url request model =
    let
        cmd =
            case Base64.decode b64State of
                Err _ ->
                    Server.Http.send <|
                        Server.Http.textResponse
                            Server.Http.badRequestStatus
                            ("State not base64 encoded: " ++ b64State)
                            request.id

                Ok state ->
                    case ED.decodeRedirectState state of
                        Err err ->
                            Server.Http.send <|
                                Server.Http.textResponse
                                    Server.Http.badRequestStatus
                                    ("Malformed state: " ++ state)
                                    request.id

                        Ok redirectState ->
                            case tokenRequest redirectState code model of
                                Err msg ->
                                    Server.Http.send <|
                                        Server.Http.textResponse
                                            Server.Http.badRequestStatus
                                            msg
                                            request.id

                                Ok tr ->
                                    Http.send
                                        (ReceiveToken
                                            request.id
                                            redirectState.redirectBackUri
                                            redirectState.scope
                                            redirectState.state
                                        )
                                        tr
    in
    ( model
    , cmd
    )


routeRequest : Result String Server.Http.Request -> Msg
routeRequest incoming =
    case incoming of
        Ok request ->
            NewRequest request

        _ ->
            NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        List.concat
            [ [ Server.Http.listen routeRequest
              , receiveFile ReceiveConfig
              ]
            , let
                period =
                    model.localConfig.configSamplePeriod
              in
              if period <= 0 then
                []

              else
                [ Time.every (toFloat period * 1000) ProbeConfig ]
            ]


main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
