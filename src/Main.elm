module Main exposing (..)

import Basics.Extra exposing (never)
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
    , flippedCards : ( Card, Card )
    , seed : Random.Seed
    }


type alias Card =
    { id : Int
    , value : String
    , faceDown : Bool
    }


blankCard : Card
blankCard =
    { id = 0, value = "?", faceDown = True }


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
      , flippedCards = ( blankCard, blankCard )
      , seed = Random.initialSeed randSeed
      }
    , newGame
    )


type Msg
    = NoOp
    | NewGame
    | FlipSingleCard Card


view : Model -> Html Msg
view model =
    div [ class "board" ]
        [ div [] <|
            List.map
                viewCard
            <|
                model.deck
        , button [ onClick NewGame ] [ text "New Game" ]
        , p [] [ text <| toString <| fst model.flippedCards ]
        , p [] [ text <| toString <| snd model.flippedCards ]
        , hr [] []
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
                        |> shuffle
            in
                ( { newModel | flippedCards = ( blankCard, blankCard ) }, Cmd.none )

        FlipSingleCard card ->
            let
                -- 1. flip card
                flipCard e =
                    if e.id == card.id then
                        { e | faceDown = not e.faceDown }
                    else
                        e

                restoreCard card flippedCards =
                    let
                        flipped1 =
                            fst flippedCards

                        flipped2 =
                            snd flippedCards
                    in
                        if card.id == flipped1.id then
                            { card | faceDown = True }
                        else if card.id == flipped2.id then
                            { card | faceDown = True }
                        else
                            card

                -- 2. add to flippedCards list
                -- updatedFlippedCards =
                --     if List.length model.flippedCards == 2 then
                --         model.flippedCards
                --     else
                --         card :: model.flippedCards
                -- move fist card in tuple to second position
                -- add click-on card to first position
                updatedFlippedCards =
                    (,) card (fst model.flippedCards)

                -- check if value of first card is equal to value of second card in tuple
                areEqual =
                    if (==) (.value <| (fst updatedFlippedCards)) (.value <| (snd updatedFlippedCards)) then
                        True
                    else
                        False

                updatedDeck =
                    -- if the last two cards flipped match, leave them face up
                    if areEqual then
                        List.map flipCard model.deck
                    else
                        List.map (\card -> restoreCard card updatedFlippedCards) model.deck

                newModel =
                    { model
                        | deck = updatedDeck
                        , flippedCards =
                            if areEqual then
                                ( blankCard, blankCard )
                            else
                                updatedFlippedCards
                    }

                -- areEqual =
                --     let
                --         card1 =
                --             Maybe.withDefault { faceDown = True, id = 0, value = "?" } <| getAt 0 updatedFlippedCards
                --
                --         card2 =
                --             Maybe.withDefault { faceDown = True, id = 0, value = "?" } <| getAt 0 updatedFlippedCards
                --     in
                --         -- if list has 2 values, continue
                --         if List.length updatedFlippedCards == 2 then
                --             card1.value == card2.value
                --             -- if list has 1 value, do nothing
                --         else
                --             False
                -- restoredDeck =
                --     List.map (\card -> if card.id == .id <| (fst updatedFlippedCards) (||) .id <| (snd updatedFlippedCards) then { model | deck = }) model.deck
                -- result =
                --     -- if cards are not equal, set faceDown = True for both
                --     -- if areEqual then
                --     --     []
                --     -- else
                --     -- List.map (\card -> { card | faceDown = True }) updatedFlippedCards
                --     if .id <| snd updatedFlippedCards == 0 then updatedFlippedCards else
            in
                -- ultimately, I want flippedCards to have 1 card in it, or none
                ( newModel, Cmd.none )



{- when user clicks on a card:
   1. flip card
   2. add to flippedCards list
   3. check length of list
       • if list has 1 value, do nothing
       • if list has 2 values, continue
   4. compare value of both cards in list
       • if cards are equal, do nothing
       • if cards are not equal, set faceDown = True for both
   5. clear flippedCards
-}
-- testSleep : Cmd Msg
-- testSleep =
--     sleep 3000 |> Task.perform (\_ -> NoOp) (\_ -> UpdateMessage)


newGame : Cmd Msg
newGame =
    sleep 0 |> Task.perform never (\_ -> NewGame)


compareCards : Card -> Card -> Bool
compareCards card1 card2 =
    card1.value == card2.value


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
