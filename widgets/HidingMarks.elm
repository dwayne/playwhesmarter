module HidingMarks exposing (main)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)

main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }

-- MODEL

type alias Flags =
    { api : String }

type alias Model =
    { api : String
    , response : Maybe PlayWheResponse
    }

type alias PlayWheResponse = List PlayWheResult

type alias PlayWheResult =
    { date : String
    , period : String
    , number : Int
    }

type Msg
    = NewResults (Result Http.Error PlayWheResponse)

init : Flags -> (Model, Cmd Msg)
init { api } =
    ( { api = api, response = Nothing }
    , getLastDraw (requestForLastDraw api)
    )

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewResults (Ok response) ->
            ( { model | response = Just response }, Cmd.none )

        NewResults (Err e) ->
            Debug.log (toString e) ( { model | response = Nothing }, Cmd.none )

-- DECODERS

decodePlayWheResponse : Decoder PlayWheResponse
decodePlayWheResponse =
    Decode.field "results" (Decode.list decodePlayWheResult)

decodePlayWheResult : Decoder PlayWheResult
decodePlayWheResult =
    Decode.map3 PlayWheResult
        (Decode.field "date" Decode.string)
        (Decode.field "period" Decode.string)
        (Decode.field "number" Decode.int)

-- HTTP

getLastDraw : Http.Request PlayWheResponse -> Cmd Msg
getLastDraw request =
    Http.send NewResults request

requestForLastDraw : String -> Http.Request PlayWheResponse
requestForLastDraw api =
    let
        endpoint = api ++ "/stats/marks-last-draw"
    in
        Http.get endpoint decodePlayWheResponse

-- VIEW

view : Model -> Html msg
view { response } =
    case response of
        Nothing ->
            div [ class "mb-5x" ] [ text "Loading..." ]

        Just response ->
            div [ class "table-responsive" ]
                [ table [ class "table table-bordered table-striped" ]
                    [ thead [ class "thead-dark" ]
                        [ tr []
                            [ th [ scope "col" ] [ text "Mark" ]
                            , th [ scope "col" ] [ text "Spirit" ]
                            , th [ scope "col" ] [ text "Date last played" ]
                            , th [ scope "col" ] [ text "Time last played" ]
                            ]
                        ]
                    , tbody [] (List.map viewResult response)
                    ]
                ]

viewResult : PlayWheResult -> Html msg
viewResult { date, period, number } =
    let
        spirit = Maybe.withDefault "unknown" (Dict.get number marks)
    in
        tr []
            [ td [] [ text (toString number) ]
            , td [] [ text spirit ]
            , td [] [ text (formatDate date) ]
            , td [] [ text (formatPeriod period) ]
            ]

-- HELPERS

marks : Dict Int String
marks = Dict.fromList
    [ (1, "centipede")
    , (2, "old lady")
    , (3, "carriage")
    , (4, "dead man")
    , (5, "parson man")
    , (6, "belly")
    , (7, "hog")
    , (8, "tiger")
    , (9, "cattle")
    , (10, "monkey")
    , (11, "corbeau")
    , (12, "king")
    , (13, "crapaud")
    , (14, "money")
    , (15, "sick woman")
    , (16, "jamette")
    , (17, "pigeon")
    , (18, "water boat")
    , (19, "horse")
    , (20, "dog")
    , (21, "mouth")
    , (22, "rat")
    , (23, "house")
    , (24, "queen")
    , (25, "morocoy")
    , (26, "fowl")
    , (27, "little snake")
    , (28, "red fish")
    , (29, "opium man")
    , (30, "house cat")
    , (31, "parson wife")
    , (32, "shrimps")
    , (33, "spider")
    , (34, "blind man")
    , (35, "big snake")
    , (36, "donkey")
    ]

months : Dict Int String
months = Dict.fromList
    [ (1, "January")
    , (2, "February")
    , (3, "March")
    , (4, "April")
    , (5, "May")
    , (6, "June")
    , (7, "July")
    , (8, "August")
    , (9, "September")
    , (10, "October")
    , (11, "November")
    , (12, "December")
    ]

formatDate : String -> String
formatDate date =
    let
        parts = String.split "-" date

        day dd =
            if String.startsWith "0" dd then
                String.dropLeft 1 dd
            else
                dd

        ordinalIndicator dd =
            case dd of
                "01" -> "st"
                "21" -> "st"
                "31" -> "st"
                "02" -> "nd"
                "22" -> "nd"
                "03" -> "rd"
                "23" -> "rd"
                _ -> "th"

        month mm =
            let
                n = Result.withDefault 0 (String.toInt mm)
            in
                Maybe.withDefault "Unknown" (Dict.get n months)

        monthAbbr mm = String.left 3 (month mm)
    in
        case parts of
            [yyyy, mm, dd] -> String.join " "
                [ monthAbbr mm
                , day dd ++ ordinalIndicator dd ++ ","
                , yyyy
                ]

            _ -> "Unknown"

formatPeriod : String -> String
formatPeriod period =
    case period of
        "EM" -> "10:30am"
        "AM" -> "1:00pm"
        "AN" -> "4:00pm"
        "PM" -> "6:30pm"
        _ -> "Unknown"
