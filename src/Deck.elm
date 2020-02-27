module Deck exposing (deck, fullDeck, main)

import Html exposing (..)
import Tuple exposing (..)


main =
    ul []
        (List.map (\card -> li [] [ text (second card ++ " of " ++ first card ++ "s") ]) fullDeck)


deck : List String
deck =
    [ "Ace", "King", "Queen", "Jack", "Ten", "Nine", "Eight", "Seven", "Six", "Five", "Four", "Three", "Two" ]


type alias Rank =
    String


type alias Suit =
    String


type alias Card =
    ( Suit, Rank )


type alias Deck =
    List Card


suits : List Suit
suits =
    [ "Spade", "Heart", "Diamond", "Club" ]


ranks : List Rank
ranks =
    [ "Ace", "King", "Queen", "Jack", "Ten", "Nine", "Eight", "Seven", "Six", "Five", "Four", "Three", "Two" ]


mapAllRanksToSuit : Suit -> List Rank -> List Card
mapAllRanksToSuit suit ranks_ =
    List.map (\rank -> ( suit, rank )) ranks_


buildFullDeck : List Suit -> List Rank -> List Card
buildFullDeck suits_ ranks_ =
    List.concatMap (\suit -> mapAllRanksToSuit suit ranks_) suits_


fullDeck : Deck
fullDeck =
    buildFullDeck suits ranks
