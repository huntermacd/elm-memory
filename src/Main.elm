module Main exposing (..)

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import Random


type alias Flags =
    { randSeed : Int }


type alias Model =
    { deck : List Card
    , flippedCards : List Card
    , seed : Random.Seed
    }


type alias Card =
    { id : Int
    , value : String
    , faceDown : Bool
    }


init : Flags -> ( Model, Cmd Msg )
init { randSeed } =
    ( { deck =
            [ { id = 1, value = "A", faceDown = True }
            , { id = 2, value = "A", faceDown = True }
            , { id = 3, value = "B", faceDown = True }
            , { id = 4, value = "B", faceDown = True }
            , { id = 5, value = "C", faceDown = True }
            , { id = 6, value = "C", faceDown = True }
            , { id = 7, value = "D", faceDown = True }
            , { id = 8, value = "D", faceDown = True }
            , { id = 9, value = "E", faceDown = True }
            , { id = 10, value = "E", faceDown = True }
            , { id = 11, value = "F", faceDown = True }
            , { id = 12, value = "F", faceDown = True }
            , { id = 13, value = "G", faceDown = True }
            , { id = 14, value = "G", faceDown = True }
            , { id = 15, value = "H", faceDown = True }
            , { id = 16, value = "H", faceDown = True }
            , { id = 17, value = "I", faceDown = True }
            , { id = 18, value = "I", faceDown = True }
            , { id = 19, value = "J", faceDown = True }
            , { id = 20, value = "J", faceDown = True }
            , { id = 21, value = "K", faceDown = True }
            , { id = 22, value = "K", faceDown = True }
            , { id = 23, value = "L", faceDown = True }
            , { id = 24, value = "L", faceDown = True }
            , { id = 25, value = "M", faceDown = True }
            , { id = 26, value = "M", faceDown = True }
            , { id = 27, value = "N", faceDown = True }
            , { id = 28, value = "N", faceDown = True }
            , { id = 29, value = "O", faceDown = True }
            , { id = 30, value = "O", faceDown = True }
            , { id = 31, value = "P", faceDown = True }
            , { id = 32, value = "P", faceDown = True }
            , { id = 33, value = "Q", faceDown = True }
            , { id = 34, value = "Q", faceDown = True }
            , { id = 35, value = "R", faceDown = True }
            , { id = 36, value = "R", faceDown = True }
            ]
      , flippedCards = []
      , seed = Random.initialSeed randSeed
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | NewGame
    | Shuffle
    | FlipCard Int


view : Model -> Html Msg
view model =
    div [ class "board" ]
        [ div [] <|
            List.map
                viewCard
            <|
                model.deck
        , button [ onClick NewGame ] [ text "New Game" ]
        , button [ onClick Shuffle ] [ text "Shuffle" ]
        ]


viewCard : Card -> Html Msg
viewCard card =
    div [ onClick <| FlipCard card.id, classList [ ( "card", True ), ( "face-down", card.faceDown ) ] ]
        [ span [ class "card-value" ] [ text <| card.value ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewGame ->
            ( { model | deck = List.map (\card -> { card | faceDown = True }) model.deck }, Cmd.none )

        Shuffle ->
            let
                randomNums =
                    Random.step (Random.list (List.length model.deck) (Random.int 1 100)) model.seed |> fst
            in
                ( { model | deck = (List.map2 (,) randomNums model.deck |> List.sortBy fst |> List.unzip |> snd) }, Cmd.none )

        FlipCard id ->
            let
                flipCard e =
                    if e.id == id then
                        { e | faceDown = not e.faceDown }
                    else
                        e
            in
                ( { model | deck = List.map flipCard model.deck }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Flags
main =
    Html.App.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
