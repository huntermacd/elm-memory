module Main exposing (..)

import Basics.Extra exposing (never)
import Debug
import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Process exposing (sleep)
import Random
import Task


type alias Flags =
    { randSeed : Int }


type alias Model =
    { deck : List Card
    , cardsToCompare : ( Card, Card )
    , seed : Random.Seed
    }


type alias Card =
    { id : Int
    , value : String
    , faceDown : Bool
    , comparing : Bool
    }


blankCard : Card
blankCard =
    { id = 0, value = "", faceDown = True, comparing = False }


init : Flags -> ( Model, Cmd Msg )
init { randSeed } =
    ( { deck =
            [ { id = 1, value = "A", faceDown = True, comparing = False }
            , { id = 2, value = "A", faceDown = True, comparing = False }
            , { id = 3, value = "B", faceDown = True, comparing = False }
            , { id = 4, value = "B", faceDown = True, comparing = False }
            , { id = 5, value = "C", faceDown = True, comparing = False }
            , { id = 6, value = "C", faceDown = True, comparing = False }
            , { id = 7, value = "D", faceDown = True, comparing = False }
            , { id = 8, value = "D", faceDown = True, comparing = False }
            , { id = 9, value = "E", faceDown = True, comparing = False }
            , { id = 10, value = "E", faceDown = True, comparing = False }
            , { id = 11, value = "F", faceDown = True, comparing = False }
            , { id = 12, value = "F", faceDown = True, comparing = False }
            , { id = 13, value = "G", faceDown = True, comparing = False }
            , { id = 14, value = "G", faceDown = True, comparing = False }
            , { id = 15, value = "H", faceDown = True, comparing = False }
            , { id = 16, value = "H", faceDown = True, comparing = False }
            , { id = 17, value = "I", faceDown = True, comparing = False }
            , { id = 18, value = "I", faceDown = True, comparing = False }
            , { id = 19, value = "J", faceDown = True, comparing = False }
            , { id = 20, value = "J", faceDown = True, comparing = False }
            , { id = 21, value = "K", faceDown = True, comparing = False }
            , { id = 22, value = "K", faceDown = True, comparing = False }
            , { id = 23, value = "L", faceDown = True, comparing = False }
            , { id = 24, value = "L", faceDown = True, comparing = False }
            , { id = 25, value = "M", faceDown = True, comparing = False }
            , { id = 26, value = "M", faceDown = True, comparing = False }
            , { id = 27, value = "N", faceDown = True, comparing = False }
            , { id = 28, value = "N", faceDown = True, comparing = False }
            , { id = 29, value = "O", faceDown = True, comparing = False }
            , { id = 30, value = "O", faceDown = True, comparing = False }
            , { id = 31, value = "P", faceDown = True, comparing = False }
            , { id = 32, value = "P", faceDown = True, comparing = False }
            , { id = 33, value = "Q", faceDown = True, comparing = False }
            , { id = 34, value = "Q", faceDown = True, comparing = False }
            , { id = 35, value = "R", faceDown = True, comparing = False }
            , { id = 36, value = "R", faceDown = True, comparing = False }
            ]
      , cardsToCompare = ( blankCard, blankCard )
      , seed = Random.initialSeed randSeed
      }
    , newGame
    )


type Msg
    = NoOp
    | NewGame
    | FlipSingleCard Card
    | FlipAllCards


view : Model -> Html Msg
view model =
    div [ class "board" ]
        [ div [] <|
            List.map
                viewCard
            <|
                model.deck
        , button [ onClick NewGame ] [ text "New Game" ]
        , ul [] <| List.map listCard model.deck
        ]


listCard card =
    li [] [ text <| toString card ]


viewCard : Card -> Html Msg
viewCard card =
    div
        [ onClick <|
            (if card.faceDown then
                FlipSingleCard
             else
                always NoOp
            )
            <|
                card
        , classList [ ( "card", True ), ( "face-down", card.faceDown ) ]
        ]
        [ Html.span [ class "card-value" ] [ text <| card.value ]
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

                -- |> shuffle
            in
                ( newModel, Cmd.none )

        FlipSingleCard card ->
            let
                flipCard e =
                    if e.id == card.id then
                        { e | faceDown = False, comparing = True }
                    else
                        e

                updatedDeck =
                    List.map flipCard model.deck

                updatedCardsToCompare =
                    (,) card (fst model.cardsToCompare)

                -- function that returns number of cards where comparing is True
                numberComparing =
                    let
                        i =
                            0
                    in
                        List.length <|
                            List.filter (\val -> val == 1) <|
                                List.map
                                    (\card ->
                                        if card.comparing == True then
                                            i + 1
                                        else
                                            i
                                    )
                                    updatedDeck

                -- if there are exactly 2 cards where comparing is True, return True
                shouldCompare =
                    if numberComparing == 2 then
                        True
                    else
                        False

                -- if shouldCompare is True, compare value of both cards in updatedCardsToCompare
                areEqual =
                    if shouldCompare then
                        (==) (.value <| fst updatedCardsToCompare) (.value <| snd updatedCardsToCompare)
                    else
                        False

                shouldRestore =
                    shouldCompare && not areEqual

                db1 =
                    Debug.log "updatedCardsToCompare" updatedCardsToCompare

                db2 =
                    Debug.log "numberComparing" numberComparing

                db3 =
                    Debug.log "shouldCompare" shouldCompare

                db4 =
                    Debug.log "areEqual" areEqual

                db5 =
                    Debug.log "shouldRestore" shouldRestore

                {-
                   If I click to "A" value cards in a row

                   updatedCardsToCompare = ("A", blank-card)
                   numberComparing = 1
                   shouldCompare = False
                   areEqual = False

                   updatedCardsToCompare = ("A", "A")
                   numberComparing = 2
                   shouldCompare = True
                   areEqual = True
                -}
            in
                -- if compared cards are equal, do nothing, else sleep 1 second, then flip all cards
                -- where comparing is True facedown
                ( { model
                    | deck = updatedDeck
                    , cardsToCompare =
                        (if not shouldCompare then
                            updatedCardsToCompare
                         else
                            ( blankCard, blankCard )
                        )
                  }
                , if shouldRestore then
                    restoreCards
                  else
                    Cmd.none
                )

        FlipAllCards ->
            let
                newModel =
                    model |> flipAllCardsFaceDown
            in
                ( newModel, Cmd.none )


restoreCards : Cmd Msg
restoreCards =
    sleep 1000 |> Task.perform never (\_ -> FlipAllCards)


newGame : Cmd Msg
newGame =
    sleep 0 |> Task.perform never (\_ -> NewGame)


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
