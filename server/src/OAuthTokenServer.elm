---------------------------------------------------------------------
--
-- OAuthTokenServer.elm
-- Top-level file for OAuthMiddleware redirectBackUri server.
-- Copyright (c) 2017-2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


port module OAuthTokenServer exposing (main, parseCodeAndState, parseErrorAndState)

import Base64
import Browser
import Debug
import Dict exposing (Dict)
import Http
import Json.Decode as JD
import Json.Encode as JE exposing (Value)
import List.Extra as LE
import OAuth
import OAuth.AuthorizationCode as AC exposing (RequestParts)
import OAuthMiddleware.EncodeDecode as ED exposing (RedirectState)
import OAuthMiddleware.ResponseToken as ResponseToken exposing (ResponseToken)
import OAuthMiddleware.ServerConfiguration
    exposing
        ( LocalServerConfiguration
        , RemoteServerConfiguration
        , configurationsDecoder
        , defaultLocalServerConfiguration
        )
import OAuthProviderSimulator
import Server.Http
import Time exposing (Posix)
import Url exposing (Url)
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
    | ReceiveToken Server.Http.Id String (List String) (Maybe String) (Result (Http.Response String) ResponseToken)


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


authenticationErrorToString : Http.Response String -> String
authenticationErrorToString response =
    case response of
        Http.BadStatus_ _ body ->
            case JD.decodeString AC.defaultAuthenticationErrorDecoder body of
                Ok { errorDescription } ->
                    case errorDescription of
                        Just desc ->
                            desc

                        Nothing ->
                            httpResponseToString response

                _ ->
                    httpResponseToString response

        _ ->
            httpResponseToString response


httpResponseToString : Http.Response String -> String
httpResponseToString response =
    case response of
        Http.BadUrl_ s ->
            "BadUrl: " ++ s

        Http.Timeout_ ->
            "Timeout"

        Http.NetworkError_ ->
            "NetworkError"

        Http.BadStatus_ metadata _ ->
            "BadStatus, code: " ++ String.fromInt metadata.statusCode

        Http.GoodStatus_ _ errorString ->
            "Decoder error: " ++ errorString


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
            in
            ( model
            , sendResponse id redirectBackUri string
            )

        NoOp ->
            ( model
            , Cmd.none
            )


sendResponse : Server.Http.Id -> String -> String -> Cmd Msg
sendResponse id redirectBackUri json =
    let
        base64 =
            Base64.encode json

        location =
            redirectBackUri ++ "#" ++ base64

        response =
            Server.Http.emptyResponse
                Server.Http.foundStatus
                id
                |> Server.Http.addHeader "location" location
    in
    Server.Http.send response


{-| The request comes from the authorization server, and is of the form:

...?code=<long code>&state=<from OAuthMiddleware.EncodeDecode.encodeRedirectState>

We can also get "?error=access\_denied&state=<foo>"

-}
newRequest : Server.Http.Request -> Model -> ( Model, Cmd Msg )
newRequest request model =
    case request.method of
        Server.Http.Post ->
            OAuthProviderSimulator.providerTokenRequest request model

        _ ->
            redirectUriRequest request model


badRequestCmd : String -> Server.Http.Request -> Cmd Msg
badRequestCmd message request =
    Server.Http.send <|
        Server.Http.textResponse
            Server.Http.badRequestStatus
            message
            request.id


fourOhFourCmd : Server.Http.Request -> Cmd Msg
fourOhFourCmd request =
    Server.Http.send <|
        Server.Http.textResponse
            Server.Http.notFoundStatus
            "404: Not Found"
            request.id


httpLocalhost : String
httpLocalhost =
    "http://localhost"


redirectUriRequest : Server.Http.Request -> Model -> ( Model, Cmd Msg )
redirectUriRequest request model =
    case Url.fromString (httpLocalhost ++ request.url) of
        Nothing ->
            -- This shouldn't happen
            ( model
            , badRequestCmd ("Can't parse url: " ++ request.url) request
            )

        Just url ->
            case parseCodeAndState url of
                Just ( code, state ) ->
                    authRequest code state url request model

                _ ->
                    case OAuthProviderSimulator.parseAuthorization url of
                        Just authorization ->
                            OAuthProviderSimulator.handleAuthorization authorization
                                request
                                model

                        Nothing ->
                            let
                                ( error, b64State ) =
                                    case parseErrorAndState url of
                                        Just res ->
                                            res

                                        _ ->
                                            ( "Missing state", "" )

                                state =
                                    Base64.decode b64State
                                        |> Result.toMaybe

                                redirectBackUri =
                                    case state of
                                        Nothing ->
                                            Nothing

                                        Just st ->
                                            case ED.decodeRedirectState st of
                                                Err err ->
                                                    Nothing

                                                Ok redirectState ->
                                                    Just redirectState.redirectBackUri
                            in
                            ( model
                            , case redirectBackUri of
                                Just uri ->
                                    sendResponse request.id uri <|
                                        ED.encodeResponseTokenError
                                            { err = error
                                            , state = state
                                            }

                                Nothing ->
                                    badRequestCmd
                                        "Bad request, missing code/state"
                                        request
                            )


{-| Adjust the default request generated by
-}
adjustRequest : (Result (Http.Response String) ResponseToken -> Msg) -> RequestParts Msg -> RequestParts Msg
adjustRequest tagger req =
    let
        headers =
            Http.header "Accept" "application/json" :: req.headers

        expect =
            Http.expectStringResponse tagger
                (authenticationResponseHandler ResponseToken.responseTokenDecoder)
    in
    { method = req.method
    , headers = headers
    , url = req.url
    , body = req.body
    , expect = expect
    , timeout = req.timeout
    , tracker = Nothing
    }


authenticationResponseHandler : JD.Decoder a -> Http.Response String -> Result (Http.Response String) a
authenticationResponseHandler decoder response =
    case response of
        Http.GoodStatus_ metadata body ->
            case JD.decodeString decoder body of
                Ok success ->
                    Ok success

                Err err ->
                    -- This denotes a decoder error
                    Err (Http.GoodStatus_ metadata <| JD.errorToString err)

        _ ->
            Err response


tokenRequest : (Result (Http.Response String) ResponseToken -> Msg) -> RedirectState -> String -> Model -> Result String (RequestParts Msg)
tokenRequest tagger { clientId, tokenUri, redirectUri, scope, redirectBackUri } code model =
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
                            Err <| "Unknown redirectBack host: " ++ host

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
                                                    -- This is never used
                                                    (\_ -> NoOp)
                                                    { credentials =
                                                        { clientId = clientId
                                                        , secret = Just clientSecret
                                                        }
                                                    , code = code
                                                    , redirectUri = redirectUrl
                                                    , url = tokenUrl
                                                    }
                                        in
                                        Ok
                                            (adjustRequest tagger requestParts)

                                    _ ->
                                        Err "Can't parse redirectUri or tokenUri"


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


{-| The signature really isn't interesting here.
-}
errorAndStateParser =
    Parser.top
        <?> Query.map2 Tuple.pair
                (Query.string "error")
                (Query.string "state")


fixUrlQuery : Url -> Url
fixUrlQuery url =
    case url.query of
        Nothing ->
            url

        Just query ->
            let
                len =
                    String.length query

                doit count =
                    if count <= 0 then
                        url

                    else
                        let
                            q =
                                String.left (len - count) query
                                    ++ String.repeat count "%3D"
                        in
                        { url | query = Just q }

                loop count =
                    if count >= len then
                        doit count

                    else
                        let
                            right =
                                String.right (count + 1) query

                            c0 =
                                String.left 1 right
                        in
                        if c0 == "=" then
                            loop (count + 1)

                        else
                            doit count
            in
            loop 0


parseErrorAndState : Url -> Maybe ( String, String )
parseErrorAndState url =
    let
        u =
            fixUrlQuery url
    in
    case Parser.parse errorAndStateParser { u | path = "" } of
        Just ( error, Just state ) ->
            let
                errmsg =
                    case error of
                        Just e ->
                            e

                        Nothing ->
                            "Missing code/state"
            in
            Just ( errmsg, state )

        _ ->
            Nothing


authRequest : String -> String -> Url -> Server.Http.Request -> Model -> ( Model, Cmd Msg )
authRequest code b64State url request model =
    let
        cmd =
            case Base64.decode b64State of
                Err _ ->
                    badRequestCmd
                        ("State not base64 encoded: " ++ b64State)
                        request

                Ok state ->
                    case ED.decodeRedirectState state of
                        Err err ->
                            badRequestCmd
                                ("Malformed state: " ++ state)
                                request

                        Ok redirectState ->
                            let
                                tagger =
                                    ReceiveToken
                                        request.id
                                        redirectState.redirectBackUri
                                        redirectState.scope
                                        redirectState.state
                            in
                            case tokenRequest tagger redirectState code model of
                                Err msg ->
                                    let
                                        ignore =
                                            Debug.log msg msg
                                    in
                                    fourOhFourCmd request

                                Ok tr ->
                                    Http.request tr
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
