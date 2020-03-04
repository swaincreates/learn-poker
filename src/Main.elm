module Main exposing (main)

import Browser
import Char exposing (fromCode)
import Deck exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode.Broken exposing (hexChar)
import List exposing (drop, take)
import Random
import Random.List exposing (shuffle)



-- Example of how to random: https://ellie-app.com/89kCDC2Zvtza1
-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, shuffleDeck initialModel.deck )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


shuffleDeck : Deck -> Cmd Msg
shuffleDeck deck =
    Random.generate ReceiveShuffled (shuffle deck)



-- MODEL


type alias Model =
    { deck : Deck
    , hand : List Card
    , communityCards : List Card
    , burnCards : List Card
    }


initialModel : Model
initialModel =
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
            ( model_, shuffleDeck model_.deck )

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
    let
        baseAttrs =
            [ style "font-size" "100px" ]

        redTextColor =
            [ style "color" "#d12d36" ]

        attrs =
            case card.suit of
                Heart ->
                    baseAttrs ++ redTextColor

                Diamond ->
                    baseAttrs ++ redTextColor

                _ ->
                    baseAttrs
    in
    div attrs [ text <| renderUnicode card ]


renderFaceDownCard : Html msg
renderFaceDownCard =
    div [ style "font-size" "100px" ] [ text <| String.fromChar '🂠' ]


strFromSuit : Card -> String
strFromSuit card =
    case card.suit of
        Spade ->
            "A"

        Heart ->
            "B"

        Diamond ->
            "C"

        Club ->
            "D"


strFromRank : Card -> String
strFromRank card =
    case card.rank of
        Ace ->
            "1"

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
            "A"

        Jack ->
            "B"

        Queen ->
            "D"

        King ->
            "E"


renderUnicode : Card -> String
renderUnicode card =
    String.fromChar <| hexChar <| "1F0" ++ strFromSuit card ++ strFromRank card


view : Model -> Html Msg
view game =
    div []
        [ button [ onClick Shuffle ] [ text "Shuffle" ]
        , button [ onClick Deal ] [ text "Deal" ]
        , button [ onClick Flop ] [ text "Flop" ]
        , button [ onClick Turn ] [ text "Turn" ]
        , button [ onClick River ] [ text "River" ]
        , h2 [] [ text "Hand" ]
        , div [ style "display" "flex" ] (List.map (\card -> renderCard card) game.hand)
        , h2 [] [ text "Community Cards" ]
        , div [ style "display" "flex" ] (List.map (\card -> renderCard card) game.communityCards)
        , h2 [] [ text "Burn Cards" ]
        , div [ style "display" "flex" ] (List.map (\card -> renderFaceDownCard) game.burnCards)

        -- , h2 [] [ text "Deck" ]
        -- , ul []
        --     (List.map (\card -> li [] [ renderCard card ]) game.deck)
        ]
