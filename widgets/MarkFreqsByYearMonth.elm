module MarkFreqsByYearMonth exposing (main)

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
  , data : RemoteData Rows
  }

type RemoteData a
  = Loading
  | Loaded a
  | Error

type alias Rows = List Row

type alias Row =
  { mark : Int
  , em : Int
  , am : Int
  , an : Int
  , pm : Int
  , total : Int
  }

type Msg
  = NewRows (Result Http.Error Rows)

init : Flags -> (Model, Cmd Msg)
init { api } =
  ( { api = api, data = Loading }
  , Http.send NewRows (getRows api)
  )

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewRows (Ok rows) ->
      ( { model | data = Loaded rows }, Cmd.none )

    NewRows (Err e) ->
      Debug.log (toString e) ( { model | data = Error }, Cmd.none )

-- DECODERS

decodeRows : Decoder Rows
decodeRows = Decode.field "counts" (Decode.list decodeRow)

decodeRow : Decoder Row
decodeRow =
  Decode.map6 Row
    (Decode.field "number" Decode.int)
    (decodeByPeriod 0)
    (decodeByPeriod 1)
    (decodeByPeriod 2)
    (decodeByPeriod 3)
    (Decode.field "count" Decode.int)

decodeByPeriod : Int -> Decoder Int
decodeByPeriod index =
  Decode.field "by_period"
    <| Decode.index index (Decode.field "count" Decode.int)

-- HTTP

getRows : String -> Http.Request Rows
getRows api =
  let
    endpoint = api ++ "/stats/mark-counts-for-date?by_period=true"
  in
    Http.get endpoint decodeRows

-- VIEW

view : Model -> Html Msg
view { data } =
  case data of
    Loading ->
      text "Loading..."

    Loaded rows ->
      viewRows rows

    Error ->
      div [ class "alert alert-danger" ]
          [ strong [] [ text "Oops! " ]
          , text "Something went wrong. Please check back in a few minutes."
          ]

viewRows : Rows -> Html msg
viewRows rows =
  div [ class "table-responsive" ]
    [ table [ class "table table-sm table-bordered table-striped" ]
      [ viewHead
      , viewBody rows
      ]
    ]

viewHead : Html msg
viewHead =
  thead [ class "thead-dark" ]
    [ tr []
      [ th [ rowspan 2 ] [ text "Mark" ]
      , th [ rowspan 2 ] [ text "Spirit" ]
      , th [ colspan 5, class "text-center border-bottom-0" ] [ text "Times played" ]
      ]
    , tr []
      [ th [ title "Morning" ] [ text "10:30am" ]
      , th [ title "Midday" ] [ text "1:00pm" ]
      , th [ title "Afternoon" ] [ text "4:00pm" ]
      , th [ title "Evening" ] [ text "6:30pm" ]
      , th [] [ text "Total" ]
      ]
    ]

viewBody : Rows -> Html msg
viewBody rows = tbody [] (List.map viewRow rows)

viewRow : Row -> Html msg
viewRow { mark, em, am, an, pm, total } =
  tr []
    [ td [] [ text (toString mark) ]
    , td [] [ text (PlayWhe.spirit mark) ]
    , td [] [ text (toString em) ]
    , td [] [ text (toString am) ]
    , td [] [ text (toString an) ]
    , td [] [ text (toString pm) ]
    , td [] [ text (toString total) ]
    ]
