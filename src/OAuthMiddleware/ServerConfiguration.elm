----------------------------------------------------------------------
--
-- ServerConfiguration.elm
-- Definition and Decoder for OAuthMiddleware server configuration.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module OAuthMiddleware.ServerConfiguration
    exposing
        ( RedirectBackHost
        , ServerConfiguration
        , redirectBackHostDecoder
        , redirectBackHostEncoder
        , serverConfigurationDecoder
        , serverConfigurationEncoder
        , serverConfigurationsDecoder
        , serverConfigurationsEncoder
        )

import Erl
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


type alias RedirectBackHost =
    { host : String
    , ssl : Bool
    }


type alias ServerConfiguration =
    { tokenUri : String
    , clientId : String
    , clientSecret : String
    , redirectBackHosts : List RedirectBackHost
    }


emptyConfig : ServerConfiguration
emptyConfig =
    { tokenUri = ""
    , clientId = ""
    , clientSecret = ""
    , redirectBackHosts = []
    }


serverConfigurationsDecoder : Decoder (List ServerConfiguration)
serverConfigurationsDecoder =
    (JD.list <|
        JD.oneOf
            [ serverConfigurationDecoder
            , commentDecoder
            ]
    )
        |> JD.map (List.filter <| (/=) emptyConfig)


commentDecoder : Decoder ServerConfiguration
commentDecoder =
    JD.field "comment" JD.string
        |> JD.andThen (\_ -> JD.succeed emptyConfig)


parseRedirectBackHost : String -> RedirectBackHost
parseRedirectBackHost string =
    let
        url =
            Erl.parse string

        host =
            String.join "." url.host
    in
    { host = host
    , ssl = url.protocol == "https"
    }


redirectBackHostDecoder : Decoder RedirectBackHost
redirectBackHostDecoder =
    JD.string |> JD.andThen (JD.succeed << parseRedirectBackHost)


serverConfigurationDecoder : Decoder ServerConfiguration
serverConfigurationDecoder =
    JD.map4 ServerConfiguration
        (JD.field "tokenUri" JD.string)
        (JD.field "clientId" JD.string)
        (JD.field "clientSecret" JD.string)
        (JD.field "redirectBackHosts" <| JD.list redirectBackHostDecoder)


serverConfigurationsEncoder : List ServerConfiguration -> Value
serverConfigurationsEncoder configs =
    JE.list <| List.map serverConfigurationEncoder configs


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


serverConfigurationEncoder : ServerConfiguration -> Value
serverConfigurationEncoder config =
    JE.object
        [ ( "tokenUri", JE.string config.tokenUri )
        , ( "clientId", JE.string config.clientId )
        , ( "clientSecret", JE.string config.clientSecret )
        , ( "redirectBackHosts"
          , JE.list <| List.map redirectBackHostEncoder config.redirectBackHosts
          )
        ]
