module Main exposing (main)

import Browser
import Html as H
import Html.Attributes as Attr
import Html.Events as Events
import Tuple
import String


main =
    Browser.sandbox
        { init = initialModel
        , update = update
        , view = view
        }



-- Model


type Model
    = StartScreen Players


type alias Players =
    ( Player, Player )


type alias Player =
    { name : String
    , color : String
    , id: Int
    }


initialModel =
    StartScreen ( { name = "Player1", color = "red", id = 1 }, { name = "Player2", color = "blue", id = 2 } )



-- Update


type Msg
    = NoOp
    | NameChanged String


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        NameChanged name ->
            model



-- View


view : Model -> H.Html Msg
view model =
    case model of
        StartScreen players ->
            defaultsForPlayersView players


defaultsForPlayersView : Players -> H.Html Msg
defaultsForPlayersView players =
    let
        mapper : Player -> H.Html Msg
        mapper player =
            H.div
                []
                [
                    H.h3 [] [H.text ("Player" ++ String.fromInt player.id ++ "'s" ++ "Name:")],
                    H.p [] [H.text player.name],
                    H.h3 [] [H.text ("Player" ++ String.fromInt player.id ++ "'s" ++ "Color:")],
                    H.p [] [H.text player.color]
                ]

        playersDefaults : (H.Html Msg, H.Html Msg)
        playersDefaults =
            Tuple.mapBoth mapper mapper players

        controls : H.Html Msg
        controls =
            H.div
                []
                [
                    H.button [] [H.text "Accept Defaults"],
                    H.button [] [H.text "Change Defaults"]
                ]
    in
        H.div
            []
            [
                H.h1 [] [H.text "Defaults For Players"],
                Tuple.first playersDefaults,
                Tuple.second playersDefaults,
                controls
            ]
