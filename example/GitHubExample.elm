----------------------------------------------------------------------
--
-- GitHubExample.elm
-- Example of using OAuthMiddleware for GitHub.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module GitHubExample exposing (..)

import Html
    exposing
        ( Attribute
        , Html
        , a
        , br
        , button
        , div
        , h2
        , input
        , option
        , p
        , select
        , span
        , table
        , td
        , text
        , textarea
        , th
        , tr
        )
import Html.Attributes
    exposing
        ( checked
        , cols
        , disabled
        , href
        , name
        , rows
        , selected
        , size
        , style
        , target
        , type_
        , value
        )
import Html.Events exposing (on, onClick, onInput, targetValue)
import Http
import Json.Decode as JD
import Json.Encode as JE exposing (Value)
import Navigation exposing (Location)
import OAuthMiddleware
    exposing
        ( Authorization
        , ResponseToken
        , TokenState(..)
        , authorize
        , locationToRedirectBackUri
        , receiveTokenAndState
        , use
        )


type alias Model =
    { token : Maybe ResponseToken
    , state : Maybe String
    , msg : Maybe String
    , user : Maybe Value
    , authorization : Authorization
    }


type Msg
    = ReceiveLocation Location
    | Login
    | GetUser
    | ReceiveUser (Result Http.Error Value)


scope : List String
scope =
    [ "user" ]


userAgentHeader : Http.Header
userAgentHeader =
    Http.header "User-Agent" "Xossbow"


authorization : Authorization
authorization =
    { authorizationUri = "https://github.com/login/oauth/authorize"
    , tokenUri = "https://github.com/login/oauth/access_token"
    , clientId = "ab3259e23daac66c0952"
    , redirectUri = "https://xossbow.com/oath/github/"
    , scope = scope
    , state = Just "Vermont"
    , redirectBackUri = "" --filled in by init
    }


main =
    Navigation.program
        ReceiveLocation
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( token, state, msg ) =
            case receiveTokenAndState location of
                TokenAndState tok stat ->
                    ( Just tok, stat, Nothing )

                TokenErrorAndState m stat ->
                    ( Nothing, stat, Just m )

                TokenDecodeError m ->
                    ( Nothing, Nothing, Just m )

                NoToken ->
                    ( Nothing, Nothing, Nothing )
    in
    { token = token
    , state = state
    , msg = msg
    , user = Nothing
    , authorization =
        { authorization
            | redirectBackUri = locationToRedirectBackUri location
        }
    }
        ! []


gitHubApiUrl : String
gitHubApiUrl =
    "https://api.github.com"


getUser : Model -> ( Model, Cmd Msg )
getUser model =
    case model.token of
        Nothing ->
            { model | msg = Just "You must login before getting user information." }
                ! []

        Just token ->
            let
                req =
                    Http.request
                        { method = "GET"
                        , headers = use token [ userAgentHeader ]
                        , url = gitHubApiUrl ++ "/user"
                        , body = Http.emptyBody
                        , expect = Http.expectJson JD.value
                        , timeout = Nothing
                        , withCredentials = False
                        }
            in
            model ! [ Http.send ReceiveUser req ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveLocation _ ->
            model ! []

        Login ->
            model ! [ authorize model.authorization ]

        GetUser ->
            getUser model

        ReceiveUser result ->
            case result of
                Err err ->
                    { model
                        | user = Nothing
                        , msg = Just <| toString err
                    }
                        ! []

                Ok user ->
                    { model
                        | user = Just user
                        , msg = Nothing
                    }
                        ! []


view : Model -> Html Msg
view model =
    div
        [ style [ ( "margin-left", "3em" ) ]
        ]
        [ h2 [] [ text "OAuthMiddleware Example" ]
        , p []
            [ button [ onClick Login ]
                [ text "Login" ]
            , text " "
            , button [ onClick GetUser ]
                [ text "Get User" ]
            ]
        , p []
            [ case ( model.msg, model.user ) of
                ( Just msg, _ ) ->
                    text <| toString msg

                ( _, Just user ) ->
                    text <| toString user

                _ ->
                    text "Nothing to report"
            ]
        ]
