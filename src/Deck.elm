module Deck exposing (..)

import Html exposing (..)


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


type Card
    = Card Rank Suit


type alias Deck =
    List Card


mapAllRanksToSuit : Suit -> List Rank -> List Card
mapAllRanksToSuit suit ranks_ =
    List.map (\rank -> Card rank suit) ranks_


buildFullDeck : List Suit -> List Rank -> List Card
buildFullDeck suits_ ranks_ =
    List.concatMap (\suit -> mapAllRanksToSuit suit ranks_) suits_


fullDeck : Deck
fullDeck =
    buildFullDeck suitEnum rankEnum
