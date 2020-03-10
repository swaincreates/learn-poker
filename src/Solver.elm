-- module Solver exposing (main)


module Solver exposing (CommunityCards, Hand, Hands, findWinningHand)

import Browser
import Deck exposing (..)
import List exposing (head)



-- main =
--     Browser.sandbox
--         { init = init
--         , view = view
--         , update = update
--         }


type alias Hand =
    List Card


type alias Hands =
    List Hand


type alias CommunityCards =
    List Card


findWinningHand : Hands -> CommunityCards -> Maybe Hand
findWinningHand hands communityCards =
    case head hands of
        Nothing ->
            Nothing

        Just hand ->
            Just hand
