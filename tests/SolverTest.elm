module SolverTest exposing (..)

import Deck exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Solver exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "The Solver module"
        [ describe "findWinningHand"
            [ test "it returns a hand or Error" <|
                \_ ->
                    let
                        hand =
                            [ Card Ace Spade, Card Ace Club ]

                        hands =
                            [ hand ]

                        communityCards : CommunityCards
                        communityCards =
                            [ Card King Diamond, Card King Spade ]
                    in
                    Expect.equal hand (findWinningHand hands communityCards)
            ]
        ]
