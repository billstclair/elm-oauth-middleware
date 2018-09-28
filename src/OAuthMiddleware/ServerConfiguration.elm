----------------------------------------------------------------------
--
-- ServerConfiguration.elm
-- Definition and Decoder for OAuthMiddleware server configuration.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module OAuthMiddleware.ServerConfiguration exposing
    ( Configurations
    , LocalServerConfiguration
    , RedirectBackHost
    , RemoteServerConfiguration
    , configurationsDecoder
    , configurationsEncoder
    , defaultLocalServerConfiguration
    , redirectBackHostDecoder
    , redirectBackHostEncoder
    )

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Url


type alias RedirectBackHost =
    { host : String
    , ssl : Bool
    }


type alias RemoteServerConfiguration =
    { tokenUri : String
    , clientId : String
    , clientSecret : String
    , redirectBackHosts : List RedirectBackHost
    }


type alias LocalServerConfiguration =
    { httpPort : Int
    , configSamplePeriod : Int
    }


type ServerConfiguration
    = Remote RemoteServerConfiguration
    | Local LocalServerConfiguration
    | Comment


emptyConfig : RemoteServerConfiguration
emptyConfig =
    { tokenUri = ""
    , clientId = ""
    , clientSecret = ""
    , redirectBackHosts = []
    }


parseRedirectBackHost : String -> Maybe RedirectBackHost
parseRedirectBackHost string =
    let
        maybeUrl =
            case Url.fromString string of
                Nothing ->
                    case Url.fromString <| "http://" ++ string of
                        Nothing ->
                            Nothing

                        x ->
                            x

                x ->
                    x
    in
    case maybeUrl of
        Nothing ->
            Nothing

        Just url ->
            let
                host =
                    url.host

                colonPort =
                    case url.port_ of
                        Nothing ->
                            ""

                        Just p ->
                            ":" ++ String.fromInt p

                authority =
                    host ++ colonPort
            in
            Just
                { host = authority
                , ssl = url.protocol == Url.Https
                }


redirectBackHostDecoder : Decoder RedirectBackHost
redirectBackHostDecoder =
    JD.string
        |> JD.andThen
            (\hostUri ->
                case parseRedirectBackHost hostUri of
                    Just host ->
                        JD.succeed host

                    Nothing ->
                        JD.fail <|
                            "Failed to parse redirect back host: "
                                ++ hostUri
            )


defaultHttpPort : Int
defaultHttpPort =
    3000


defaultConfigSamplePeriod : Int
defaultConfigSamplePeriod =
    2


defaultLocalServerConfiguration : LocalServerConfiguration
defaultLocalServerConfiguration =
    { httpPort = defaultHttpPort
    , configSamplePeriod = defaultConfigSamplePeriod
    }


serverConfigurationsDecoder : Decoder (List ServerConfiguration)
serverConfigurationsDecoder =
    JD.list serverConfigurationDecoder


type alias Configurations =
    { local : LocalServerConfiguration
    , remote : List RemoteServerConfiguration
    }


splitLocalRemote : List ServerConfiguration -> ( List LocalServerConfiguration, List RemoteServerConfiguration )
splitLocalRemote configs =
    let
        loop confs local remote =
            case confs of
                [] ->
                    ( List.reverse local, List.reverse remote )

                (Local loc) :: rest ->
                    loop rest (loc :: local) remote

                (Remote remo) :: rest ->
                    loop rest local (remo :: remote)

                _ :: rest ->
                    loop rest local remote
    in
    loop configs [] []


serverConfigurationsToConfigurations : List ServerConfiguration -> Decoder Configurations
serverConfigurationsToConfigurations configs =
    let
        ( locals, remotes ) =
            splitLocalRemote configs
    in
    case locals of
        _ :: _ :: _ ->
            JD.fail "Multiple local configurations."

        _ ->
            JD.succeed
                { local =
                    case locals of
                        [] ->
                            defaultLocalServerConfiguration

                        loc :: _ ->
                            loc
                , remote = remotes
                }


configurationsDecoder : Decoder Configurations
configurationsDecoder =
    serverConfigurationsDecoder
        |> JD.andThen serverConfigurationsToConfigurations


serverConfigurationDecoder : Decoder ServerConfiguration
serverConfigurationDecoder =
    JD.oneOf
        [ JD.map (\_ -> Comment)
            (JD.field "comment" JD.value)
        , JD.map4 RemoteServerConfiguration
            (JD.field "tokenUri" JD.string)
            (JD.field "clientId" JD.string)
            (JD.field "clientSecret" JD.string)
            (JD.field "redirectBackHosts" <| JD.list redirectBackHostDecoder)
            |> JD.map Remote
        , JD.map2 LocalServerConfiguration
            (JD.oneOf
                [ JD.field "port" JD.int
                , JD.succeed defaultHttpPort
                ]
            )
            (JD.oneOf
                [ JD.field "configSamplePeriod" JD.int
                , JD.succeed defaultConfigSamplePeriod
                ]
            )
            |> JD.map Local
        ]


configurationsEncoder : Configurations -> Value
configurationsEncoder configs =
    localConfigurationEncoder configs.local
        :: List.map remoteConfigurationEncoder configs.remote
        |> JE.list identity


redirectBackHostEncoder : RedirectBackHost -> Value
redirectBackHostEncoder host =
    let
        https =
            if host.ssl then
                "https://"

            else
                ""
    in
    JE.string <| https ++ host.host


localConfigurationEncoder : LocalServerConfiguration -> Value
localConfigurationEncoder config =
    JE.object
        [ ( "port", JE.int config.httpPort )
        , ( "configSamplePeriod", JE.int config.configSamplePeriod )
        ]


remoteConfigurationEncoder : RemoteServerConfiguration -> Value
remoteConfigurationEncoder config =
    JE.object
        [ ( "tokenUri", JE.string config.tokenUri )
        , ( "clientId", JE.string config.clientId )
        , ( "clientSecret", JE.string config.clientSecret )
        , ( "redirectBackHosts"
          , JE.list redirectBackHostEncoder config.redirectBackHosts
          )
        ]
