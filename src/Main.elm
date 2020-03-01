module Main exposing (main)

import Browser
import Deck exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)
import List exposing (drop, take)
import Random
import Random.List exposing (shuffle)



-- Example of how to random: https://ellie-app.com/89kCDC2Zvtza1
-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( model, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { deck : Deck
    , hand : List Card
    , communityCards : List Card
    , burnCards : List Card
    }


model : Model
model =
    { deck = fullDeck
    , hand = []
    , communityCards = []
    , burnCards = []
    }



-- UPDATE


type Msg
    = Deal
    | Flop
    | Turn
    | River
    | Shuffle
    | ReceiveShuffled Deck


deal : Model -> Model
deal game =
    { deck = drop 2 game.deck
    , hand = take 2 game.deck
    , communityCards = []
    , burnCards = []
    }


dealOut : Int -> Model -> Model
dealOut num game =
    { game
        | deck = drop (num + 1) game.deck
        , communityCards = game.communityCards ++ (game.deck |> drop 1 |> take num)
        , burnCards = game.burnCards ++ take 1 game.deck
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model_ =
    case msg of
        Shuffle ->
            ( model_, Random.generate ReceiveShuffled (shuffle model_.deck) )

        ReceiveShuffled shuffledDeck ->
            ( { model_ | deck = shuffledDeck }, Cmd.none )

        Deal ->
            ( deal model_, Cmd.none )

        Flop ->
            ( dealOut 3 model_, Cmd.none )

        Turn ->
            ( dealOut 1 model_, Cmd.none )

        River ->
            ( dealOut 1 model_, Cmd.none )



-- VIEW


renderCard : Card -> Html msg
renderCard card =
    text ("[" ++ renderRankFromCard card ++ renderEmojiFromCard card ++ "]")


renderEmojiFromCard : Card -> String
renderEmojiFromCard card =
    case card.suit of
        Spade ->
            "♠️"

        Club ->
            "♣️️"

        Diamond ->
            "♦️"

        Heart ->
            "♥️"


renderRankFromCard : Card -> String
renderRankFromCard card =
    case .rank card of
        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"

        Six ->
            "6"

        Seven ->
            "7"

        Eight ->
            "8"

        Nine ->
            "9"

        Ten ->
            "10"

        Jack ->
            "J"

        Queen ->
            "Q"

        King ->
            "K"

        Ace ->
            "A"


view : Model -> Html Msg
view game =
    div []
        [ button [ onClick Shuffle ] [ text "Shuffle" ]
        , button [ onClick Deal ] [ text "Deal" ]
        , button [ onClick Flop ] [ text "Flop" ]
        , button [ onClick Turn ] [ text "Turn" ]
        , button [ onClick River ] [ text "River" ]
        , h2 [] [ text "Hand" ]
        , ul []
            (List.map (\card -> li [] [ renderCard card ]) game.hand)
        , h2 [] [ text "Community Cards" ]
        , ul []
            (List.map (\card -> li [] [ renderCard card ]) game.communityCards)
        , h2 [] [ text "Burn Cards" ]
        , ul []
            (List.map (\card -> li [] [ renderCard card ]) game.burnCards)
        , h2 [] [ text "Deck" ]
        , ul []
            (List.map (\card -> li [] [ renderCard card ]) game.deck)
        ]
