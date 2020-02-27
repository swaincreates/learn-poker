module Main exposing (main)

import Browser
import Deck exposing (deck)
import Html exposing (..)
import Html.Events exposing (onClick)
import List exposing (drop, tail, take)



-- MAIN


main =
    Browser.sandbox
        { init = model
        , update = update
        , view = view
        }



-- MODEL


type alias Game =
    { deck : List String
    , hand : List String
    , communityCards : List String
    , burnCards : List String
    }


model : Game
model =
    { deck = deck
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


deal : Game -> Game
deal game =
    { deck = drop 2 game.deck
    , hand = take 2 game.deck
    , communityCards = []
    , burnCards = []
    }


dealOut : Int -> Game -> Game
dealOut num game =
    { game
        | deck = drop (num + 1) game.deck
        , communityCards = game.communityCards ++ (game.deck |> drop 1 |> take num)
        , burnCards = game.burnCards ++ take 1 game.deck
    }


update : Msg -> Game -> Game
update msg game =
    case msg of
        Deal ->
            deal game

        Flop ->
            dealOut 3 game

        Turn ->
            dealOut 1 game

        River ->
            dealOut 1 game



-- VIEW


view : Game -> Html Msg
view game =
    div []
        [ h2 [] [ text "Deck" ]
        , ul []
            (List.map (\card -> li [] [ text card ]) game.deck)
        , h2 [] [ text "Hand" ]
        , ul []
            (List.map (\card -> li [] [ text card ]) game.hand)
        , h2 [] [ text "Community Cards" ]
        , ul []
            (List.map (\card -> li [] [ text card ]) game.communityCards)
        , h2 [] [ text "Burn Cards" ]
        , ul []
            (List.map (\card -> li [] [ text card ]) game.burnCards)
        , button [ onClick Deal ] [ text "Deal" ]
        , button [ onClick Flop ] [ text "Flop" ]
        , button [ onClick Turn ] [ text "Turn" ]
        , button [ onClick River ] [ text "River" ]
        ]
