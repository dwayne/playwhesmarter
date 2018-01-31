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
  { api : String
  , data : Remote Data
  }

type Remote a
  = Loading
  | Loaded a
  | Error

type Data
  = Sunday
  | Data Suggestions

type alias Suggestions =
  { likelyMarks : List (List Int)
  , allLikelyMarks : List Int
  , unlikelyMarks : List (List Int)
  , allUnlikelyMarks : List Int
  , likelyLines : List (List Int)
  , allLikelyLines : List Int
  , unlikelyLines : List (List Int)
  , allUnlikelyLines : List Int
  }

type Msg = NewData (Result Http.Error Data)

init : Flags -> (Model, Cmd Msg)
init { api } =
  ( { api = api, data = Loading }
  , sendDataReq api
  )

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewData (Ok data) ->
      ( { model | data = Loaded data }, Cmd.none )

    NewData (Err e) ->
      Debug.log (toString e) ( { model | data = Error }, Cmd.none)

-- DECODERS

decodeData : Decoder Data
decodeData =
  Decode.field "is_sunday" Decode.bool
    |> Decode.andThen
      (\isSunday ->
        if isSunday then
          Decode.succeed Sunday
        else
          Decode.map Data decodeSuggestions
      )

decodeSuggestions : Decoder Suggestions
decodeSuggestions =
  Decode.map8 Suggestions
    (Decode.field "likely_marks" decodeByPeriod)
    (Decode.field "all_likely_marks" decodeIntList)
    (Decode.field "unlikely_marks" decodeByPeriod)
    (Decode.field "all_unlikely_marks" decodeIntList)
    (Decode.field "likely_lines" decodeByPeriod)
    (Decode.field "all_likely_lines" decodeIntList)
    (Decode.field "unlikely_lines" decodeByPeriod)
    (Decode.field "all_unlikely_lines" decodeIntList)

decodeByPeriod : Decoder (List (List Int))
decodeByPeriod = Decode.list decodePicks

decodePicks : Decoder (List Int)
decodePicks = Decode.field "picks" decodeIntList

decodeIntList : Decoder (List Int)
decodeIntList = Decode.list Decode.int

-- HTTP

sendDataReq : String -> Cmd Msg
sendDataReq api = Http.send NewData (dataReq api)

dataReq : String -> Http.Request Data
dataReq api = Http.get (api ++ "/suggestions") decodeData

-- VIEW

view : Model -> Html Msg
view { data } =
  case data of
    Loading ->
      div [ class "mb-5x text-center" ] [ text "Please wait while we generate today's favourites and backups..." ]

    Loaded Sunday ->
      div [ class "mb-5x" ]
        [ div [ class "row" ]
          [ div [ class "col-md-8 offset-md-2" ]
            [ div [ class "alert alert-info text-center" ]
              [ text "Today is Sunday! So there's no Play Whe today. Check back tomorrow for your favourites and backups." ]
            ]
          ]
        ]

    Loaded (Data { likelyMarks, allLikelyMarks, unlikelyMarks, allUnlikelyMarks, likelyLines, allLikelyLines, unlikelyLines, allUnlikelyLines }) ->
      div []
        [ viewSuggestions "Favourites" "Favourite" likelyMarks allLikelyMarks likelyLines allLikelyLines
        , viewSuggestions "Backups" "Backup" unlikelyMarks allUnlikelyMarks unlikelyLines allUnlikelyLines
        ]

    Error ->
      div [ class "mb-5x" ]
        [ div [ class "row" ]
          [ div [ class "col-md-8 offset-md-2" ]
            [ div [ class "alert alert-danger text-center" ]
              [ text "We seem to be experiencing some technical difficulties at the moment. Please check back for today's favourites and backups in about 5 minutes. Hopefully the problem would be resolved by then." ]
            ]
          ]
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
