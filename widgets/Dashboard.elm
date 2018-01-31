module Dashboard exposing (main)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import PlayWhe

main : Program Flags Model Msg
main = Html.programWithFlags
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
  , results : Remote PlayWheResults
  }

type Remote a
  = Loading
  | Loaded a
  | Error

type alias PlayWheResults = List PlayWheResult

type alias PlayWheResult =
  { draw : Int
  , date : String
  , period : String
  , number : Int
  }

type Msg = NewPlayWheResults (Result Http.Error PlayWheResults)

init : Flags -> (Model, Cmd Msg)
init { api } =
  ( { api = api, results = Loading }
  , sendPlayWheResultsReq api
  )

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewPlayWheResults (Ok results) ->
      ( { model | results = Loaded results }, Cmd.none )

    NewPlayWheResults (Err e) ->
      Debug.log (toString e) ( { model | results = Error }, Cmd.none)

-- DECODERS

decodePlayWheResults : Decoder PlayWheResults
decodePlayWheResults = Decode.field "results" (Decode.list decodePlayWheResult)

decodePlayWheResult : Decoder PlayWheResult
decodePlayWheResult =
  Decode.map4 PlayWheResult
    (Decode.field "draw" Decode.int)
    (Decode.field "date" Decode.string)
    (Decode.field "period" Decode.string)
    (Decode.field "number" Decode.int)

-- HTTP

sendPlayWheResultsReq : String -> Cmd Msg
sendPlayWheResultsReq api = Http.send NewPlayWheResults (playWheResultsReq api)

playWheResultsReq : String -> Http.Request PlayWheResults
playWheResultsReq api = Http.get (api ++ "/results?limit=4") decodePlayWheResults

-- VIEW

view : Model -> Html Msg
view { results } =
  case results of
    Loading ->
      div [ class "mb-4 text-center" ] [ text "Loading..." ]

    Loaded results ->
      div [ class "d-flex flex-row flex-wrap justify-content-around" ]
        (List.map viewResult results)

    Error ->
      div [ class "mb-4" ]
        [ div [ class "row" ]
          [ div [ class "col-md-8 offset-md-2" ]
            [ div [ class "alert alert-danger text-center" ]
              [ text "We seem to be experiencing some technical difficulties at the moment. Please check back in about 5 minutes. Hopefully the problem would be resolved by then." ]
            ]
          ]
        ]

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
        [ small [] [ text (PlayWhe.formatDate date ++ " at " ++ PlayWhe.formatPeriod period) ] ]

viewNumber : Int -> Html Msg
viewNumber number =
  div [] [ strong [ class "display-2" ] [ text (toString number) ] ]

viewSpirit : Int -> Html Msg
viewSpirit number =
  div [] [ small [] [ text (PlayWhe.spirit number) ] ]

viewDraw : Int -> Html Msg
viewDraw draw =
  div [] [ small [ class "text-muted" ] [ text (toString draw) ] ]
