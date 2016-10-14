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
            [ { id = 1, value = "A", faceDown = False }
            , { id = 2, value = "A", faceDown = False }
            , { id = 3, value = "B", faceDown = False }
            , { id = 4, value = "B", faceDown = False }
            , { id = 5, value = "C", faceDown = False }
            , { id = 6, value = "C", faceDown = False }
            , { id = 7, value = "D", faceDown = False }
            , { id = 8, value = "D", faceDown = False }
            , { id = 9, value = "E", faceDown = False }
            , { id = 10, value = "E", faceDown = False }
            , { id = 11, value = "F", faceDown = False }
            , { id = 12, value = "F", faceDown = False }
            , { id = 13, value = "G", faceDown = False }
            , { id = 14, value = "G", faceDown = False }
            , { id = 15, value = "H", faceDown = False }
            , { id = 16, value = "H", faceDown = False }
            , { id = 17, value = "I", faceDown = False }
            , { id = 18, value = "I", faceDown = False }
            , { id = 19, value = "J", faceDown = False }
            , { id = 20, value = "J", faceDown = False }
            , { id = 21, value = "K", faceDown = False }
            , { id = 22, value = "K", faceDown = False }
            , { id = 23, value = "L", faceDown = False }
            , { id = 24, value = "L", faceDown = False }
            , { id = 25, value = "M", faceDown = False }
            , { id = 26, value = "M", faceDown = False }
            , { id = 27, value = "N", faceDown = False }
            , { id = 28, value = "N", faceDown = False }
            , { id = 29, value = "O", faceDown = False }
            , { id = 30, value = "O", faceDown = False }
            , { id = 31, value = "P", faceDown = False }
            , { id = 32, value = "P", faceDown = False }
            , { id = 33, value = "Q", faceDown = False }
            , { id = 34, value = "Q", faceDown = False }
            , { id = 35, value = "R", faceDown = False }
            , { id = 36, value = "R", faceDown = False }
            ]
      , flippedCards = []
      , seed = Random.initialSeed randSeed
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | NewGame
    | FlipSingleCard Int


view : Model -> Html Msg
view model =
    div [ class "board" ]
        [ div [] <|
            List.map
                viewCard
            <|
                model.deck
        , button [ onClick NewGame ] [ text "New Game" ]
        ]


viewCard : Card -> Html Msg
viewCard card =
    div [ onClick <| FlipSingleCard card.id, classList [ ( "card", True ), ( "face-down", card.faceDown ) ] ]
        [ span [ class "card-value" ] [ text <| card.value ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewGame ->
            let
                newModel =
                    model
                        |> flipAllCardsFaceDown
                        |> shuffle
            in
                ( newModel, Cmd.none )

        FlipSingleCard id ->
            let
                flipCard e =
                    if e.id == id then
                        { e | faceDown = not e.faceDown }
                    else
                        e
            in
                ( { model | deck = List.map flipCard model.deck }, Cmd.none )


flipAllCardsFaceDown : Model -> Model
flipAllCardsFaceDown model =
    { model | deck = List.map (\card -> { card | faceDown = True }) model.deck }


shuffle : Model -> Model
shuffle model =
    let
        randomNums =
            Random.step (Random.list (List.length model.deck) (Random.int 1 100)) model.seed |> fst
    in
        { model | deck = List.map2 (,) randomNums model.deck |> List.sortBy fst |> List.unzip |> snd }


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
