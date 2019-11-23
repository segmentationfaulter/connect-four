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


type PlayerID
    = One
    | Two


type alias Player =
    { name : String
    , color : String
    , id : PlayerID
    }


initialModel =
    ShowingDefaults ( { name = "Player1", color = "#FF0000", id = One }, { name = "Player2", color = "#0000FF", id = Two } )



-- Update


type Msg
    = ProceedToChangeDefaults
    | PlayerNameChanged PlayerID String
    | PlayerColorChanged PlayerID String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ProceedToChangeDefaults ->
            case model of
                ShowingDefaults players ->
                    ShowingFormToChangeDefaults players

                ShowingFormToChangeDefaults players ->
                    model

        PlayerNameChanged id name ->
            case id of
                One ->
                    case model of
                        ShowingDefaults _ ->
                            model

                        ShowingFormToChangeDefaults ( p1, p2 ) ->
                            ShowingFormToChangeDefaults ( { p1 | name = name }, p2 )

                Two ->
                    case model of
                        ShowingDefaults _ ->
                            model

                        ShowingFormToChangeDefaults ( p1, p2 ) ->
                            ShowingFormToChangeDefaults ( p1, { p2 | name = name } )

        PlayerColorChanged id color ->
            case id of
                One ->
                    case model of
                        ShowingDefaults _ ->
                            model

                        ShowingFormToChangeDefaults ( p1, p2 ) ->
                            ShowingFormToChangeDefaults ( { p1 | color = color }, p2 )

                Two ->
                    case model of
                        ShowingDefaults _ ->
                            model

                        ShowingFormToChangeDefaults ( p1, p2 ) ->
                            ShowingFormToChangeDefaults ( p1, { p2 | color = color } )



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
                [ H.h3 [] [ H.text ("Player" ++ String.fromInt (mapPlayerIdToNumber player.id) ++ "'s" ++ "Name:") ]
                , H.p [] [ H.text player.name ]
                , H.h3 [] [ H.text ("Player" ++ String.fromInt (mapPlayerIdToNumber player.id) ++ "'s" ++ "Color:") ]
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
                [ H.legend [] [ H.text ("Make changes to defaults of Player" ++ String.fromInt (mapPlayerIdToNumber player.id)) ]
                , H.label []
                    [ H.text "Name: "
                    , H.input [ Attr.type_ "text", Attr.value player.name, Events.onInput (PlayerNameChanged player.id) ] []
                    ]
                , H.label []
                    [ H.text "Color: "
                    , H.input [ Attr.type_ "color", Attr.value player.color, Events.onInput (PlayerColorChanged player.id) ] []
                    ]
                ]

        fieldsets : ( H.Html Msg, H.Html Msg )
        fieldsets =
            Tuple.mapBoth fieldsetViewForPlayer fieldsetViewForPlayer players

        validationError : Maybe String
        validationError =
            let
                playersNamesMatch =
                    (Tuple.first players).name == (Tuple.second players).name

                playersColorsMatch =
                    (Tuple.first players).color == (Tuple.second players).color
            in
            if playersColorsMatch then
                Just "Both players cannot have same colors"

            else if playersNamesMatch then
                Just "Both players cannot have same names"

            else
                Nothing

        validationMessage : H.Html Msg
        validationMessage =
            case validationError of
                Just error ->
                    H.text error

                Nothing ->
                    H.text ""

        disableSubmitButton : Bool
        disableSubmitButton =
            case validationError of
                Just _ ->
                    True

                Nothing ->
                    False
    in
    H.form
        []
        [ Tuple.first fieldsets
        , Tuple.second fieldsets
        , H.input [ Attr.type_ "submit", Attr.value "Submit change and proceed to game", Attr.disabled disableSubmitButton ] []
        , H.p [] [ validationMessage ]
        ]



-- Helpers


mapPlayerIdToNumber : PlayerID -> Int
mapPlayerIdToNumber id =
    case id of
        One ->
            1

        Two ->
            2
