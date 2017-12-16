port module Main exposing (main)

import Debug
import Dict exposing (Dict)
import Erl
import Erl.Query as Q
import Http
import Json.Decode as JD
import OAuth exposing (Authentication(..))
import OAuth.AuthorizationCode
import OAuthMiddleware
import Platform
import Server.Http
import Time exposing (Time)


port getFile : String -> Cmd msg


port receiveFile : (Maybe String -> msg) -> Sub msg


type alias Configuration =
    { tokenUri : String
    , clientId : String
    , clientSecret : String
    , redirectBackHosts : List String
    }


type alias ClientInfo =
    { tokenUri : String
    , clientId : String
    , clientSecret : String
    }


type alias RedirectDict =
    Dict String ClientInfo


addConfigToDict : Configuration -> RedirectDict -> RedirectDict
addConfigToDict config dict =
    let
        info =
            { tokenUri = config.tokenUri
            , clientId = config.clientId
            , clientSecret = config.clientSecret
            }
    in
    List.foldl
        (\host dict ->
            Dict.insert host info dict
        )
        dict
        config.redirectBackHosts


buildRedirectDict : List Configuration -> RedirectDict
buildRedirectDict configs =
    List.foldl addConfigToDict Dict.empty configs


type alias Model =
    { configString : String
    , config : List Configuration
    , redirectDict : RedirectDict
    }


type Msg
    = NoOp
    | ReceiveConfig (Maybe String)
    | ProbeConfig Time
    | NewRequest Server.Http.Request
    | ReceiveToken Server.Http.Id String (Maybe String) (Result Http.Error OAuth.ResponseToken)


init : ( Model, Cmd Msg )
init =
    ( { configString = ""
      , config = []
      , redirectDict = Dict.empty
      }
    , getConfig
    )


configFile : String
configFile =
    "build/config.json"


getConfig : Cmd msg
getConfig =
    getFile configFile


emptyConfig : Configuration
emptyConfig =
    { tokenUri = ""
    , clientId = ""
    , clientSecret = ""
    , redirectBackHosts = []
    }


configsDecoder : JD.Decoder (List Configuration)
configsDecoder =
    (JD.list <|
        JD.oneOf
            [ configDecoder
            , commentDecoder
            ]
    )
        |> JD.map (List.filter <| (/=) emptyConfig)


commentDecoder : JD.Decoder Configuration
commentDecoder =
    JD.field "comment" JD.string
        |> JD.andThen (\_ -> JD.succeed emptyConfig)


configDecoder : JD.Decoder Configuration
configDecoder =
    JD.map4 Configuration
        (JD.field "tokenUri" JD.string)
        (JD.field "clientId" JD.string)
        (JD.field "clientSecret" JD.string)
        (JD.field "redirectBackHosts" <| JD.list JD.string)


receiveConfig : Maybe String -> Model -> ( Model, Cmd Msg )
receiveConfig file model =
    case file of
        Nothing ->
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
            model ! []

        Just json ->
            if json == model.configString then
                model ! []
            else
                case JD.decodeString configsDecoder json of
                    Err msg ->
                        let
                            m =
                                Debug.log "Error" msg
                        in
                        receiveConfig Nothing model

                    Ok config ->
                        let
                            m =
                                if config == [] then
                                    Debug.log "Notice"
                                        "Empty configuration disables server."
                                else
                                    Debug.log "Notice"
                                        ("Successfully parsed " ++ configFile)
                        in
                        { model
                            | configString = json
                            , config = config
                            , redirectDict = buildRedirectDict config
                        }
                            ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveConfig file ->
            receiveConfig file model

        ProbeConfig _ ->
            model ! [ getConfig ]

        NewRequest request ->
            newRequest request model

        ReceiveToken id redirectBackUri state result ->
            let
                ( key, value ) =
                    case result of
                        Err err ->
                            ( OAuthMiddleware.responseTokenQueryError
                            , toString err
                            )

                        Ok responseToken ->
                            ( OAuthMiddleware.responseTokenQuery
                            , OAuthMiddleware.encodeResponseToken
                                { responseToken | state = state }
                            )

                query =
                    Q.add key value []
                        --is html query escaping correct here?
                        |> Q.toString

                location =
                    redirectBackUri ++ query

                response =
                    Server.Http.emptyResponse
                        Server.Http.foundStatus
                        id
                        |> Server.Http.addHeader "location" location
            in
            model
                ! [ Server.Http.send response ]

        NoOp ->
            model ! []


{-| The request comes from the authorization server, and is of the form:

...?code=<long code>&state=<from OAuthMiddleware.encodeRedirectState>

We can also get "?error=access_denied&state=<foo>"

-}
newRequest : Server.Http.Request -> Model -> ( Model, Cmd Msg )
newRequest request model =
    let
        url =
            Erl.parse request.url

        host =
            url.host

        query =
            url.query

        codes =
            Q.getValuesForKey "code" query

        states =
            Q.getValuesForKey "state" query
    in
    case ( codes, states ) of
        ( [ code ], [ state ] ) ->
            authRequest code state url request model

        _ ->
            model
                ! [ Server.Http.send <|
                        Server.Http.textResponse
                            Server.Http.badRequestStatus
                            "Bad request, missing code/state"
                            request.id
                  ]


tokenRequest : String -> List String -> String -> String -> Model -> Result String (Http.Request OAuth.ResponseToken)
tokenRequest redirectUri scope redirectBackUri code model =
    let
        host =
            Erl.extractHost redirectBackUri
    in
    case Dict.get host model.redirectDict of
        Nothing ->
            Err <| "Unknown redirect host: " ++ host

        Just { tokenUri, clientId, clientSecret } ->
            Ok <|
                OAuth.AuthorizationCode.authenticate <|
                    AuthorizationCode
                        { credentials =
                            { clientId = clientId
                            , secret = clientSecret
                            }
                        , code = code
                        , redirectUri = redirectUri
                        , scope = scope
                        , state = Nothing --don't need to get this back
                        , url = tokenUri
                        }


authRequest : String -> String -> Erl.Url -> Server.Http.Request -> Model -> ( Model, Cmd Msg )
authRequest code state url request model =
    let
        cmd =
            case OAuthMiddleware.decodeRedirectState state of
                Err err ->
                    Server.Http.send <|
                        Server.Http.textResponse
                            Server.Http.badRequestStatus
                            ("Malformed state: " ++ state)
                            request.id

                Ok { redirectUri, scope, redirectBackUri, state } ->
                    case tokenRequest redirectUri scope redirectBackUri code model of
                        Err err ->
                            Server.Http.send <|
                                Server.Http.textResponse
                                    Server.Http.badRequestStatus
                                    ("Unknown redirectBackUri: " ++ redirectBackUri)
                                    request.id

                        Ok tokenRequest ->
                            Http.send
                                (ReceiveToken request.id redirectBackUri state)
                                tokenRequest
    in
    model ! [ cmd ]


routeRequest : Result String Server.Http.Request -> Msg
routeRequest incoming =
    case incoming of
        Ok request ->
            NewRequest request

        _ ->
            NoOp


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Server.Http.listen routeRequest
        , receiveFile ReceiveConfig
        , Time.every (2 * Time.second) ProbeConfig
        ]


main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
