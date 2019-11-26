module Main exposing (..)

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
    | ShowingGameBoard Players FirstMover Grid


type alias Players =
    ( Player, Player )


type PlayerID
    = One
    | Two


type alias FirstMover =
    PlayerID


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
    | ProceedToPlay
    | AddDisk ColumnIndex


update : Msg -> Model -> Model
update msg model =
    case msg of
        ProceedToChangeDefaults ->
            case model of
                ShowingDefaults players ->
                    ShowingFormToChangeDefaults players

                ShowingFormToChangeDefaults players ->
                    model

                ShowingGameBoard _ _ _ ->
                    model

        PlayerNameChanged id name ->
            case id of
                One ->
                    case model of
                        ShowingDefaults _ ->
                            model

                        ShowingFormToChangeDefaults ( p1, p2 ) ->
                            ShowingFormToChangeDefaults ( { p1 | name = name }, p2 )

                        ShowingGameBoard _ _ _ ->
                            model

                Two ->
                    case model of
                        ShowingDefaults _ ->
                            model

                        ShowingFormToChangeDefaults ( p1, p2 ) ->
                            ShowingFormToChangeDefaults ( p1, { p2 | name = name } )

                        ShowingGameBoard _ _ _ ->
                            model

        PlayerColorChanged id color ->
            case id of
                One ->
                    case model of
                        ShowingDefaults _ ->
                            model

                        ShowingFormToChangeDefaults ( p1, p2 ) ->
                            ShowingFormToChangeDefaults ( { p1 | color = color }, p2 )

                        ShowingGameBoard _ _ _ ->
                            model

                Two ->
                    case model of
                        ShowingDefaults _ ->
                            model

                        ShowingFormToChangeDefaults ( p1, p2 ) ->
                            ShowingFormToChangeDefaults ( p1, { p2 | color = color } )

                        ShowingGameBoard _ _ _ ->
                            model

        ProceedToPlay ->
            case model of
                ShowingDefaults players ->
                    ShowingGameBoard players One initialGrid

                ShowingFormToChangeDefaults players ->
                    ShowingGameBoard players One initialGrid

                ShowingGameBoard _ _ _ ->
                    model

        AddDisk colIndex ->
            case model of
                ShowingDefaults _ -> model
                ShowingFormToChangeDefaults _ -> model
                ShowingGameBoard players mover currentGrid ->
                    let
                        updatedGrid = addDisk mover colIndex currentGrid
                        switchedMover =
                            case mover of
                                One -> Two
                                Two -> One
                    in
                    ShowingGameBoard players switchedMover updatedGrid
                    




-- View


view : Model -> H.Html Msg
view model =
    case model of
        ShowingDefaults players ->
            defaultsForPlayersView players

        ShowingFormToChangeDefaults players ->
            formViewToChangeDefaults players

        ShowingGameBoard players mover grid ->
            drawGrid grid players



--- Start screen


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
                [ H.button [ Events.onClick ProceedToPlay ] [ H.text "Accept Defaults" ]
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
        [ Events.onSubmit ProceedToPlay ]
        [ Tuple.first fieldsets
        , Tuple.second fieldsets
        , H.input [ Attr.type_ "submit", Attr.value "Submit change and proceed to game", Attr.disabled disableSubmitButton ] []
        , H.p [] [ validationMessage ]
        ]



-- Grid


gridHeight =
    6


gridWidth =
    7


type alias ColumnIndex =
    Int


type alias RowIndex =
    Int


type alias Slot =
    { filledBy : PlayerID
    , rowIndex : RowIndex
    }


type alias SlotUnderTest =
    { filledBy : PlayerID
    , index : Int
    }


type Column
    = Column (List Slot) ColumnIndex


type alias Grid =
    List Column


initialGrid : Grid
initialGrid =
    List.repeat gridWidth [] |> List.indexedMap (\colIndex column -> Column column colIndex)


addDisk : PlayerID -> ColumnIndex -> Grid -> Grid
addDisk pid targetIndex grid =
    let
        mapper : Column -> Column
        mapper (Column slots colIndex) =
            let
                rowIndex =
                    List.length slots

                newSlot =
                    Slot pid rowIndex
            in
            if colIndex == targetIndex then
                Column (newSlot :: slots) colIndex

            else
                Column slots colIndex
    in
    List.map mapper grid


getHorizontalSlotsToTest : RowIndex -> Grid -> List SlotUnderTest
getHorizontalSlotsToTest rowIndex grid =
    let
        mapper : Column -> List SlotUnderTest
        mapper (Column slots colIndex) =
            slots
                |> List.filter (\slot -> slot.rowIndex == rowIndex)
                |> List.map (\slot -> { filledBy = slot.filledBy, index = colIndex })
    in
    grid
        |> List.map mapper
        |> List.concat


getVerticalSlotsToTest : ColumnIndex -> Grid -> List SlotUnderTest
getVerticalSlotsToTest targetIndex grid =
    let
        targetColumn =
            List.filter (\(Column slots colIndex) -> colIndex == targetIndex) grid
    in
    case targetColumn of
        [] ->
            []

        (Column slots _) :: tail ->
            List.map (\slot -> { filledBy = slot.filledBy, index = slot.rowIndex }) slots



-- Right diagonal is the one which links bottom left to top right


getIndexesOfRightDiagonal : ColumnIndex -> RowIndex -> List ( ColumnIndex, RowIndex )
getIndexesOfRightDiagonal targetColumnIndex targetRowIndex =
    let
        upperHalfOfIndexes : ColumnIndex -> RowIndex -> List ( ColumnIndex, RowIndex ) -> List ( ColumnIndex, RowIndex )
        upperHalfOfIndexes colIndex rowIndex accumulator =
            if colIndex < gridWidth && rowIndex < gridHeight then
                upperHalfOfIndexes (colIndex + 1) (rowIndex + 1) (accumulator ++ [ ( colIndex, rowIndex ) ])

            else
                accumulator

        lowerHalfOfIndexes : ColumnIndex -> RowIndex -> List ( ColumnIndex, RowIndex ) -> List ( ColumnIndex, RowIndex )
        lowerHalfOfIndexes colIndex rowIndex accumulator =
            if colIndex > -1 && rowIndex > -1 then
                lowerHalfOfIndexes (colIndex - 1) (rowIndex - 1) (( colIndex, rowIndex ) :: accumulator)

            else
                accumulator
    in
    lowerHalfOfIndexes targetColumnIndex targetRowIndex [] ++ upperHalfOfIndexes (targetColumnIndex + 1) (targetRowIndex + 1) []



-- Left diagonal is the one which link bottom right to top left


getIndexesOfLeftDiagonal : ColumnIndex -> RowIndex -> List ( ColumnIndex, RowIndex )
getIndexesOfLeftDiagonal targetColumnIndex targetRowIndex =
    let
        upperHalfOfIndexes : ColumnIndex -> RowIndex -> List ( ColumnIndex, RowIndex ) -> List ( ColumnIndex, RowIndex )
        upperHalfOfIndexes colIndex rowIndex accumulator =
            if colIndex > -1 && rowIndex < gridHeight then
                upperHalfOfIndexes (colIndex - 1) (rowIndex + 1) (accumulator ++ [ ( colIndex, rowIndex ) ])

            else
                accumulator

        lowerHalfOfIndexes : ColumnIndex -> RowIndex -> List ( ColumnIndex, RowIndex ) -> List ( ColumnIndex, RowIndex )
        lowerHalfOfIndexes colIndex rowIndex accumulator =
            if colIndex < gridWidth && rowIndex > -1 then
                lowerHalfOfIndexes (colIndex + 1) (rowIndex - 1) (( colIndex, rowIndex ) :: accumulator)

            else
                accumulator
    in
    lowerHalfOfIndexes (targetColumnIndex + 1) (targetRowIndex - 1) [] ++ upperHalfOfIndexes targetColumnIndex targetRowIndex []


wonAgainstSlotsUnderTest : List SlotUnderTest -> Bool
wonAgainstSlotsUnderTest slots =
    let
        helper : List SlotUnderTest -> { streak : Int, lastIndex : Maybe Int } -> Int
        helper slots_ { streak, lastIndex } =
            case slots_ of
                head :: tail ->
                    case lastIndex of
                        Just index ->
                            if abs (head.index - index) == 1 then
                                helper tail { streak = streak + 1, lastIndex = Just head.index }

                            else
                                helper tail { streak = 1, lastIndex = Just head.index }

                        Nothing ->
                            helper tail { streak = 1, lastIndex = Just head.index }

                [] ->
                    streak
    in
    if helper slots { streak = 0, lastIndex = Nothing } > 3 then
        True

    else
        False


drawGrid : Grid -> Players -> H.Html Msg
drawGrid grid players =
    let
        drawSlot : Slot -> H.Html Msg
        drawSlot slot =
            let
                { color } = getPlayerById slot.filledBy players
            in
            
            H.div
                [ Attr.class "slot", Attr.style "border-color" color ]
                []

        drawColumn : Column -> H.Html Msg
        drawColumn (Column slots colIndex) =
            H.div
                [ Attr.class "column", Attr.title "Click to add a disk", Events.onClick (AddDisk colIndex)]
                (List.map drawSlot slots)
    in
    H.div
        [ Attr.class "grid" ]
        (List.map drawColumn grid)



-- Helpers


mapPlayerIdToNumber : PlayerID -> Int
mapPlayerIdToNumber id =
    case id of
        One ->
            1

        Two ->
            2

getPlayerById : PlayerID -> Players -> Player
getPlayerById pid players =
    case pid of
        One -> Tuple.first players 
        Two -> Tuple.second players
