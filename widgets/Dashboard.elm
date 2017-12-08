module Dashboard exposing (main)

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
    { results : Maybe PlayWheResponse
    , api : String
    }

type alias PlayWheResponse =
    List PlayWheResult

type alias PlayWheResult =
    { draw : Int
    , date : String
    , period : String
    , number : Int
    }

type Msg
    = LatestResults (Result Http.Error PlayWheResponse)

init : Flags -> (Model, Cmd Msg)
init { api } =
    ( { results = Nothing, api = api }
    , getResults (requestForLatest api)
    )

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LatestResults (Ok response) ->
            ( { model | results = Just response }, Cmd.none )

        LatestResults (Err e) ->
            Debug.log (toString e) ( { model | results = Nothing }, Cmd.none)

decodePlayWheResponse : Decoder PlayWheResponse
decodePlayWheResponse =
    Decode.field "results" (Decode.list decodePlayWheResult)

decodePlayWheResult : Decoder PlayWheResult
decodePlayWheResult =
    Decode.map4 PlayWheResult
        (Decode.field "draw" Decode.int)
        (Decode.field "date" Decode.string)
        (Decode.field "period" Decode.string)
        (Decode.field "number" Decode.int)

getResults : Http.Request PlayWheResponse -> Cmd Msg
getResults request =
    Http.send LatestResults request

requestForLatest : String -> Http.Request PlayWheResponse
requestForLatest api =
    let
        endpoint = api ++ "/results?limit=4"
    in
        Http.get endpoint decodePlayWheResponse

-- VIEW

view : Model -> Html Msg
view { results } =
    case results of
        Nothing ->
            div [ class "mb-4 text-center" ] [ text "Loading..." ]

        Just results ->
            div [ class "d-flex flex-row flex-wrap justify-content-around" ]
                (List.map viewResult results)

viewResult : PlayWheResult -> Html Msg
viewResult { draw, date, period, number } =
    div [ class "mb-4 px-2" ]
        [ div
            [ class "d-inline-block p-2 text-center border border-secondary rounded bg-light"
            , style [ ("width", "200px") ]
            ]
            [ viewDate date period
            , viewNumber number
            , viewSpirit number
            , viewDraw draw
            ]
        ]

viewDate : String -> String -> Html Msg
viewDate date period =
    div []
        [ small [] [ text (formatDate date ++ " at " ++ formatPeriod period) ] ]

formatDate : String -> String
formatDate date =
    let
        parts = String.split "-" date

        monthAbbr mm = Maybe.withDefault "Unknown" (Dict.get mm monthAbbrs)

        day dd =
            if String.startsWith "0" dd then
                String.dropLeft 1 dd
            else
                dd
    in
        case parts of
            [yyyy, mm, dd] -> String.join " "
                [ monthAbbr mm
                , day dd ++ ","
                , yyyy
                ]

            _ -> "Unknown"

monthAbbrs : Dict String String
monthAbbrs = Dict.fromList
    [ ("01", "Jan")
    , ("02", "Feb")
    , ("03", "Mar")
    , ("04", "Apr")
    , ("05", "May")
    , ("06", "Jun")
    , ("07", "Jul")
    , ("08", "Aug")
    , ("09", "Sep")
    , ("10", "Oct")
    , ("11", "Nov")
    , ("12", "Dec")
    ]

formatPeriod : String -> String
formatPeriod period =
    case period of
        "EM" -> "10:30am"
        "AM" -> "1:00pm"
        "AN" -> "4:00pm"
        "PM" -> "6:30pm"
        _ -> "Unknown"

viewNumber : Int -> Html Msg
viewNumber number =
    div [] [ strong [ class "display-2" ] [ text (toString number) ] ]

viewSpirit : Int -> Html Msg
viewSpirit number =
    let
        spirit = Maybe.withDefault "unknown" (Dict.get number marks)
    in
        div [] [ small [] [ text spirit ] ]

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

viewDraw : Int -> Html Msg
viewDraw draw =
    div [] [ small [ class "text-muted" ] [ text (toString draw) ] ]
