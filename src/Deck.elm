module Deck exposing (..)

import Html exposing (..)
import Random exposing (..)
import Random.List exposing (shuffle)
import Tuple exposing (..)


main =
    ul []
        (List.map (\card -> li [] [ text <| Debug.toString card ]) fullDeck)


suitEnum : List Suit
suitEnum =
    [ Club, Diamond, Heart, Spade ]


type Suit
    = Club
    | Diamond
    | Heart
    | Spade


rankEnum : List Rank
rankEnum =
    [ Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King ]


type Rank
    = Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King


type alias Card =
    { suit : Suit, rank : Rank }


type alias Deck =
    List Card


newDeck =
    [ { suit = Diamond, rank = Two }, { suit = Heart, rank = Ace } ]


mapAllRanksToSuit : Suit -> List Rank -> List Card
mapAllRanksToSuit suit ranks_ =
    List.map (\rank -> { suit = suit, rank = rank }) ranks_


buildFullDeck : List Suit -> List Rank -> List Card
buildFullDeck suits_ ranks_ =
    List.concatMap (\suit -> mapAllRanksToSuit suit ranks_) suits_


fullDeck =
    buildFullDeck suitEnum rankEnum
