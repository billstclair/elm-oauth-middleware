module Main exposing (main)

import Config
import Dict exposing (Dict)
import Erl
import Erl.Query
import Http
import OAuth exposing (Authentication(..))
import OAuth.AuthorizationCode
import OAuthMiddleware
import Platform
import Server.Http


config : List Config.Configuration
config =
    Config.configuration


type alias ClientInfo =
    { tokenUri : String
    , clientId : String
    , clientSecret : String
    }


type alias RedirectDict =
    Dict String ClientInfo


addConfigToDict : Config.Configuration -> RedirectDict -> RedirectDict
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


buildRedirectDict : List Config.Configuration -> RedirectDict
buildRedirectDict configs =
    List.foldl addConfigToDict Dict.empty configs


type alias Model =
    { redirectDict : RedirectDict
    }


type Msg
    = NoOp
    | NewRequest Server.Http.Request
    | ReceiveToken Server.Http.Id String (Maybe String) (Result Http.Error OAuth.ResponseToken)


init : ( Model, Cmd Msg )
init =
    ( { redirectDict = buildRedirectDict config
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewRequest request ->
            newRequest request model

        ReceiveToken id redirectBackUri state result ->
            model
                ! [ Server.Http.send <|
                        Server.Http.textResponse
                            Server.Http.badRequestStatus
                            (toString ( redirectBackUri, state, result ))
                            id
                  ]

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
            Erl.Query.getValuesForKey "code" query

        states =
            Erl.Query.getValuesForKey "state" query
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
        ]


main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
