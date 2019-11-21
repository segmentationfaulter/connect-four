module Main exposing (main)

import Browser
import Html as H
import Html.Attributes as Attr
import Html.Events as Events
import String
import Tuple


main =
    Browser.sandbox
        { init = initialModel
        , update = update
        , view = view
        }



-- Model


type Model
    = ShowingDefaults Players
    | ShowingFormToChangeDefaults Players


type alias Players =
    ( Player, Player )


type alias Player =
    { name : String
    , color : String
    , id : Int
    }


playersDefaults =
    ( { name = "Player1", color = "#FF0000", id = 1 }, { name = "Player2", color = "#0000FF", id = 2 } )


initialModel =
    ShowingDefaults playersDefaults



-- Update


type Msg
    = ProceedToChangeDefaults


update : Msg -> Model -> Model
update msg model =
    case msg of
        ProceedToChangeDefaults ->
            ShowingFormToChangeDefaults playersDefaults



-- View


view : Model -> H.Html Msg
view model =
    case model of
        ShowingDefaults players ->
            defaultsForPlayersView players

        ShowingFormToChangeDefaults players ->
            formViewToChangeDefaults players


defaultsForPlayersView : Players -> H.Html Msg
defaultsForPlayersView players =
    let
        mapper : Player -> H.Html Msg
        mapper player =
            H.div
                []
                [ H.h3 [] [ H.text ("Player" ++ String.fromInt player.id ++ "'s" ++ "Name:") ]
                , H.p [] [ H.text player.name ]
                , H.h3 [] [ H.text ("Player" ++ String.fromInt player.id ++ "'s" ++ "Color:") ]
                , H.p [] [ H.text player.color ]
                ]

        playersDefaultsView : ( H.Html Msg, H.Html Msg )
        playersDefaultsView =
            Tuple.mapBoth mapper mapper players

        controls : H.Html Msg
        controls =
            H.div
                []
                [ H.button [] [ H.text "Accept Defaults" ]
                , H.button [ Events.onClick ProceedToChangeDefaults ] [ H.text "Change Defaults" ]
                ]
    in
    H.div
        []
        [ H.h1 [] [ H.text "Defaults For Players" ]
        , Tuple.first playersDefaultsView
        , Tuple.second playersDefaultsView
        , controls
        ]


formViewToChangeDefaults : Players -> H.Html Msg
formViewToChangeDefaults players =
    let
        fieldsetViewForPlayer : Player -> H.Html Msg
        fieldsetViewForPlayer player =
            H.fieldset
                []
                [ H.legend [] [ H.text ("Make changes to defaults of " ++ player.name) ]
                , H.label []
                    [ H.text "Name: "
                    , H.input [ Attr.type_ "text" ] []
                    ]
                , H.label []
                    [ H.text "Color: "
                    , H.input [ Attr.type_ "color", Attr.value player.color ] []
                    ]
                ]

        fieldsets : ( H.Html Msg, H.Html Msg )
        fieldsets =
            Tuple.mapBoth fieldsetViewForPlayer fieldsetViewForPlayer players
    in
    H.form
        []
        [ Tuple.first fieldsets
        , Tuple.second fieldsets
        , H.input [ Attr.type_ "submit", Attr.value "Submit change and proceed to game" ] []
        ]
