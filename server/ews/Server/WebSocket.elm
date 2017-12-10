port module Server.WebSocket exposing (Id, Msg(..), compareId, disconnect, listen, send)

import Json.Decode as D exposing (Decoder)
import Json.Encode as E


port websocketIn : (D.Value -> msg) -> Sub msg


port websocketOut : E.Value -> Cmd msg



-- Id


type Id
    = Id String


compareId : Id -> Id -> Bool
compareId (Id a) (Id b) =
    a == b



-- Msg


type Msg
    = Connected Id
    | Disconnected Id
    | Message Id String


decoder : Decoder Msg
decoder =
    D.oneOf
        [ D.map2
            Message
            (D.field "from" (D.map Id D.string))
            (D.field "message" D.string)
        , D.map
            Connected
            (D.field "connected" (D.map Id D.string))
        , D.map
            Disconnected
            (D.field "disconnected" (D.map Id D.string))
        ]


encodeMessage : String -> Id -> E.Value
encodeMessage message (Id id) =
    E.object
        [ ( "to", E.string id )
        , ( "message", E.string message )
        ]


encodeDisconnection : Id -> E.Value
encodeDisconnection (Id id) =
    E.object
        [ ( "disconnect", E.string id )
        ]


listen : (Result String Msg -> msg) -> Sub msg
listen msg =
    websocketIn (msg << D.decodeValue decoder)


send : String -> Id -> Cmd msg
send message id =
    (websocketOut << encodeMessage message) id


disconnect : Id -> Cmd msg
disconnect id =
    (websocketOut << encodeDisconnection) id
