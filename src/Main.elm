module Main exposing (main)

import Browser
import Html as H
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Json
import List


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
    }


initialModel =
    StartScreen ( { name = "Player1", color = "red" }, { name = "Player2", color = "blue" } )



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
            startScreenView players


startScreenView : Players -> H.Html Msg
startScreenView ( p1, p2 ) =
    let
        innerHtmlDecoder : Json.Decoder String
        innerHtmlDecoder =
            Json.at [ "target", "innerHTML" ] Json.string

        colorSelector : List String -> H.Html Msg
        colorSelector colors =
            H.select []
                (List.map (\color -> H.option [ Attr.value color ] [ H.text color ]) colors)

        playerEditor : Player -> H.Html Msg
        playerEditor player =
            H.div []
                [ H.div []
                    [ H.span
                        [ Attr.contenteditable True
                        , Events.on "input" (Json.map NameChanged innerHtmlDecoder)
                        ]
                        [ H.text player.name ]
                    , colorSelector [ "red", "blue", "green" ]
                    ]
                ]
    in
    H.div []
        [ playerEditor p1
        ]
