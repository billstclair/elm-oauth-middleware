port module Server.Http
    exposing
        ( Id
        , Method(..)
        , Request
        , Response
        , Status
        , badRequestStatus
        , emptyResponse
        , htmlResponse
        , internalErrorStatus
        , jsonResponse
        , listen
        , notFoundStatus
        , okStatus
        , send
        , textResponse
        , unauthorizedStatus
        )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Server.Html as Html


port httpIn : (D.Value -> msg) -> Sub msg


port httpOut : E.Value -> Cmd msg



-- Id


type Id
    = Id String



-- Request


type alias Request =
    { id : Id
    , method : Method
    , headers : Dict String String
    , url : String
    , body : Maybe String
    }


type Method
    = Get
    | Put
    | Post
    | Delete
    | Other String


parseMethod : String -> Method
parseMethod text =
    case String.toLower text of
        "get" ->
            Get

        "put" ->
            Put

        "post" ->
            Post

        "delete" ->
            Delete

        _ ->
            Other text


requestDecoder : Decoder Request
requestDecoder =
    D.map5
        Request
        (D.field "id" (D.map Id D.string))
        (D.field "method" (D.map parseMethod D.string))
        (D.field "headers" (D.map Dict.fromList (D.keyValuePairs D.string)))
        (D.field "url" D.string)
        (D.field "body" (D.maybe D.string))


listen : (Result String Request -> msg) -> Sub msg
listen msg =
    httpIn (msg << D.decodeValue requestDecoder)



-- Response


type Response
    = Response
        { id : Id
        , status : Status
        , headers : Dict String String
        , body : Maybe String
        }


htmlResponse : Status -> Html.Document -> Id -> Response
htmlResponse status html id =
    Response
        { id = id
        , status = status
        , headers = Dict.fromList [ (,) "content-type" "text/html" ]
        , body = Just (Html.toString html)
        }


textResponse : Status -> String -> Id -> Response
textResponse status text id =
    Response
        { id = id
        , status = status
        , headers = Dict.fromList [ (,) "content-type" "text/plain" ]
        , body = Just text
        }


jsonResponse : Status -> E.Value -> Id -> Response
jsonResponse status json id =
    Response
        { id = id
        , status = status
        , headers = Dict.fromList [ (,) "content-type" "application/json" ]
        , body = Just (E.encode 0 json)
        }


emptyResponse : Status -> Id -> Response
emptyResponse status id =
    Response
        { id = id
        , status = status
        , headers = Dict.empty
        , body = Nothing
        }


encodeResponse : Response -> E.Value
encodeResponse (Response { id, status, headers, body }) =
    let
        (Id idString) =
            id
    in
    E.object
        [ ( "id", E.string idString )
        , ( "status"
          , E.object
                [ ( "code", E.int status.code )
                , ( "message", E.string status.message )
                ]
          )
        , ( "headers", (E.object << (List.map << Tuple.mapSecond) E.string << Dict.toList) headers )
        , ( "body", (Maybe.withDefault E.null << Maybe.map E.string) body )
        ]


send : Response -> Cmd msg
send response =
    (httpOut << encodeResponse) response



-- Status


type alias Status =
    { code : Int
    , message : String
    }


okStatus : Status
okStatus =
    Status 200 "ok"


badRequestStatus : Status
badRequestStatus =
    Status 400 "bad request"


unauthorizedStatus : Status
unauthorizedStatus =
    Status 401 "unauthorized"


notFoundStatus : Status
notFoundStatus =
    Status 404 "not found"


internalErrorStatus : Status
internalErrorStatus =
    Status 500 "internal server error"
