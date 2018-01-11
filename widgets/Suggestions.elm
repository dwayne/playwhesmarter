module Suggestions exposing (main)

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
    { suggestions : Maybe PlayWheResponse
    , api : String
    }

type alias PlayWheResponse =
    { likelyMarks : List (List Int)
    , allLikelyMarks : List Int
    , unlikelyMarks : List (List Int)
    , allUnlikelyMarks : List Int
    , likelyLines : List (List Int)
    , allLikelyLines : List Int
    , unlikelyLines : List (List Int)
    , allUnlikelyLines : List Int
    }

type Msg
    = NewSuggestions (Result Http.Error PlayWheResponse)

init : Flags -> (Model, Cmd Msg)
init { api } =
    ( { suggestions = Nothing, api = api }
    , getSuggestions (requestForSuggestions api)
    )

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewSuggestions (Ok response) ->
            ( { model | suggestions = Just response }, Cmd.none )

        NewSuggestions (Err e) ->
            Debug.log (toString e) ( { model | suggestions = Nothing }, Cmd.none)

-- DECODERS

decodePlayWheResponse : Decoder PlayWheResponse
decodePlayWheResponse =
    Decode.map8 PlayWheResponse
        (Decode.field "likely_marks" decodeByPeriod)
        (Decode.field "all_likely_marks" decodeIntList)
        (Decode.field "unlikely_marks" decodeByPeriod)
        (Decode.field "all_unlikely_marks" decodeIntList)
        (Decode.field "likely_lines" decodeByPeriod)
        (Decode.field "all_likely_lines" decodeIntList)
        (Decode.field "unlikely_lines" decodeByPeriod)
        (Decode.field "all_unlikely_lines" decodeIntList)

decodeByPeriod : Decoder (List (List Int))
decodeByPeriod =
    Decode.list decodePicks

decodePicks : Decoder (List Int)
decodePicks =
    Decode.field "picks" decodeIntList

decodeIntList : Decoder (List Int)
decodeIntList =
    Decode.list Decode.int

-- HTTP

getSuggestions : Http.Request PlayWheResponse -> Cmd Msg
getSuggestions request =
    Http.send NewSuggestions request

requestForSuggestions : String -> Http.Request PlayWheResponse
requestForSuggestions api =
    let
        endpoint = api ++ "/suggestions"
    in
        Http.get endpoint decodePlayWheResponse

-- VIEW

view : Model -> Html Msg
view { suggestions } =
    case suggestions of
        Nothing ->
            div [ class "mb-5x text-center" ] [ text "Please wait while we generate today's favourites and backups..." ]

        Just { likelyMarks, allLikelyMarks, unlikelyMarks, allUnlikelyMarks, likelyLines, allLikelyLines, unlikelyLines, allUnlikelyLines } ->
            div []
                [ viewSuggestions "Favourites" "Favourite" likelyMarks allLikelyMarks likelyLines allLikelyLines
                , viewSuggestions "Backups" "Backup" unlikelyMarks allUnlikelyMarks unlikelyLines allUnlikelyLines
                ]

viewSuggestions : String -> String -> List (List Int) -> List Int -> List (List Int) -> List Int -> Html Msg
viewSuggestions title prefix marks allMarks lines allLines =
    let
        header = thead [ class "thead-dark" ]
            [ tr []
                [ th [ scope "col" ] [ text "Time of day" ]
                , th [ scope "col" ] [ text (prefix ++ " marks") ]
                , th [ scope "col" ] [ text (prefix ++ " lines") ]
                ]
            ]

        toRow tod ns ls = tr []
            [ td [] [ text tod ]
            , td [] [ text (commaSep ns) ]
            , td [] [ text (commaSep ls) ]
            ]

        rows = List.map3 toRow timesOfDay marks lines
    in
        div [ class "mb-5x" ]
            [ h2 [ class "mb-3 text-center" ] [ text ("Today's " ++ title) ]
            , div [ class "row" ]
                [ div [ class "col-md-6 offset-md-3" ]
                    [ div [ class "table-responsive" ]
                        [ table [ class "table table-sm table-bordered" ]
                            [ tr []
                                [ th [ scope "row", class "table-dark" ] [ text (prefix ++ " marks:") ]
                                , td [] [ text (commaSep allMarks) ]
                                ]
                            , tr []
                                [ th [ scope "row", class "table-dark" ] [ text (prefix ++ " lines:") ]
                                , td [] [ text (commaSep allLines) ]
                                ]
                            ]
                        ]
                    ]
                ]
            , h3 [ class "h4 text-center" ] [ text "Coming under:" ]
            , div [ class "row" ]
                [ div [ class "col-md-8 offset-md-2" ]
                    [ div [ class "table-responsive" ]
                        [ table [ class "table table-sm table-bordered" ]
                            (List.append [header] rows)
                        ]
                    ]
                ]
            ]

commaSep : List Int -> String
commaSep numbers = String.join ", " (List.map toString numbers)

timesOfDay : List String
timesOfDay =
    [ "Morning (10:30am)"
    , "Midday (1:00pm)"
    , "Afternoon (4:00pm)"
    , "Evening (6:30pm)"
    ]
