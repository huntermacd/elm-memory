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
    , seed : Random.Seed
    }


type alias Card =
    { value : String
    , faceDown : Bool
    }


init : Flags -> ( Model, Cmd Msg )
init { randSeed } =
    ( { deck =
            [ { value = "A", faceDown = True }
            , { value = "A", faceDown = True }
            , { value = "B", faceDown = True }
            , { value = "B", faceDown = True }
            , { value = "C", faceDown = True }
            , { value = "C", faceDown = True }
            , { value = "D", faceDown = True }
            , { value = "D", faceDown = True }
            , { value = "E", faceDown = True }
            , { value = "E", faceDown = True }
            , { value = "F", faceDown = True }
            , { value = "F", faceDown = True }
            , { value = "G", faceDown = True }
            , { value = "G", faceDown = True }
            , { value = "H", faceDown = True }
            , { value = "H", faceDown = True }
            , { value = "I", faceDown = True }
            , { value = "I", faceDown = True }
            , { value = "J", faceDown = True }
            , { value = "J", faceDown = True }
            , { value = "K", faceDown = True }
            , { value = "K", faceDown = True }
            , { value = "L", faceDown = True }
            , { value = "L", faceDown = True }
            , { value = "M", faceDown = True }
            , { value = "M", faceDown = True }
            , { value = "N", faceDown = True }
            , { value = "N", faceDown = True }
            , { value = "O", faceDown = True }
            , { value = "O", faceDown = True }
            , { value = "P", faceDown = True }
            , { value = "P", faceDown = True }
            , { value = "Q", faceDown = True }
            , { value = "Q", faceDown = True }
            , { value = "R", faceDown = True }
            , { value = "R", faceDown = True }
            ]
      , seed = Random.initialSeed randSeed
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | NewGame


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
    div [ class "card" ]
        [ div [ class "card-value" ] [ text <| card.value ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewGame ->
            let
                randomNums =
                    Random.step (Random.list (List.length model.deck) (Random.int 1 100)) model.seed |> fst
            in
                ( { model | deck = (List.map2 (,) randomNums model.deck |> List.sortBy fst |> List.unzip |> snd) }, Cmd.none )


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
