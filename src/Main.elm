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
    | ShowingGameBoard Players GameState Grid


type alias Players =
    ( Player, Player )


type PlayerID
    = One
    | Two


type GameState
    = NextMover PlayerID
    | Winner PlayerID
    | Draw


type alias Player =
    { name : String
    , color : String
    , id : PlayerID
    }


type alias ColumnIndex =
    Int


type alias RowIndex =
    Int


type alias LastInsertionMadeAt =
    RowIndex


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


gridHeight =
    6


gridWidth =
    7


winningStreak =
    4


initialModel =
    ShowingDefaults ( { name = "Player1", color = "#FF0000", id = One }, { name = "Player2", color = "#0000FF", id = Two } )


initialGrid : Grid
initialGrid =
    List.repeat gridWidth [] |> List.indexedMap (\colIndex column -> Column column colIndex)



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
                    ShowingGameBoard players (NextMover One) initialGrid

                ShowingFormToChangeDefaults players ->
                    ShowingGameBoard players (NextMover One) initialGrid

                ShowingGameBoard _ _ _ ->
                    model

        AddDisk colIndex ->
            case model of
                ShowingDefaults _ ->
                    model

                ShowingFormToChangeDefaults _ ->
                    model

                ShowingGameBoard players gameState currentGrid ->
                    case gameState of
                        NextMover pid ->
                            let
                                updatedGrid =
                                    addDisk pid colIndex currentGrid

                                moverWonIt =
                                    didMoverWonIt colIndex pid updatedGrid

                                slotsVacantInGrid =
                                    List.any (\(Column slots _) -> List.length slots < gridHeight) updatedGrid

                                nextMover =
                                    case pid of
                                        One ->
                                            Two

                                        Two ->
                                            One
                            in
                            if moverWonIt then
                                ShowingGameBoard players (Winner pid) updatedGrid

                            else if not moverWonIt && slotsVacantInGrid then
                                ShowingGameBoard players (NextMover nextMover) updatedGrid

                            else
                                ShowingGameBoard players Draw updatedGrid

                        Draw ->
                            model

                        Winner _ ->
                            model



-- View


view : Model -> H.Html Msg
view model =
    case model of
        ShowingDefaults players ->
            defaultsForPlayersView players

        ShowingFormToChangeDefaults players ->
            formViewToChangeDefaults players

        ShowingGameBoard players mover grid ->
            H.div
                [ Attr.class "game-view" ]
                [ drawGrid players mover grid
                , gameInfo players mover grid
                ]



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


addDisk : PlayerID -> ColumnIndex -> Grid -> Grid
addDisk pid targetIndex grid =
    let
        mapper : Column -> Column
        mapper (Column slots colIndex) =
            let
                occupiedSlots =
                    List.length slots

                newSlot =
                    Slot pid occupiedSlots

                hasVacantSlot =
                    occupiedSlots < gridHeight
            in
            if colIndex == targetIndex && hasVacantSlot then
                Column (newSlot :: slots) colIndex

            else
                Column slots colIndex
    in
    List.map mapper grid


getHorizontalSlotsToTest : RowIndex -> PlayerID -> Grid -> List SlotUnderTest
getHorizontalSlotsToTest rowIndex pid grid =
    let
        mapper : Column -> List SlotUnderTest
        mapper (Column slots colIndex) =
            slots
                |> List.filter (\slot -> slot.rowIndex == rowIndex && slot.filledBy == pid)
                |> List.map (\slot -> { filledBy = slot.filledBy, index = colIndex })
    in
    grid
        |> List.map mapper
        |> List.concat


getVerticalSlotsToTest : PlayerID -> List Slot -> List SlotUnderTest
getVerticalSlotsToTest pid slots =
    slots
        |> List.map (\slot -> { filledBy = slot.filledBy, index = slot.rowIndex })
        |> List.filter (\slot -> slot.filledBy == pid)



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
                upperHalfOfIndexes (colIndex - 1) (rowIndex + 1) (( colIndex, rowIndex ) :: accumulator)

            else
                accumulator

        lowerHalfOfIndexes : ColumnIndex -> RowIndex -> List ( ColumnIndex, RowIndex ) -> List ( ColumnIndex, RowIndex )
        lowerHalfOfIndexes colIndex rowIndex accumulator =
            if colIndex < gridWidth && rowIndex > -1 then
                lowerHalfOfIndexes (colIndex + 1) (rowIndex - 1) (accumulator ++ [ ( colIndex, rowIndex ) ])

            else
                accumulator
    in
    upperHalfOfIndexes targetColumnIndex targetRowIndex [] ++ lowerHalfOfIndexes (targetColumnIndex + 1) (targetRowIndex - 1) []


getSlotsToTestOnDiagonal : List ( ColumnIndex, RowIndex ) -> PlayerID -> Grid -> List SlotUnderTest
getSlotsToTestOnDiagonal indices pid grid =
    let
        ( columnIndices, rowIndices ) =
            List.unzip indices

        targetColumns =
            List.filter (\(Column _ colIndex) -> List.member colIndex columnIndices) grid

        mapper : Column -> RowIndex -> List SlotUnderTest
        mapper (Column slots colIndex) rowIndex =
            slots
                |> List.filter (\slot -> slot.rowIndex == rowIndex && slot.filledBy == pid)
                |> List.map (\slot -> { filledBy = slot.filledBy, index = colIndex })
    in
    List.map2 mapper targetColumns rowIndices
        |> List.concat


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


didMoverWonIt : ColumnIndex -> PlayerID -> Grid -> Bool
didMoverWonIt targetColumnIndex pid grid =
    let
        targetColumnInList =
            List.filter (\(Column _ colIndex) -> colIndex == targetColumnIndex) grid

        horizontalSlotsToTest =
            case targetColumnInList of
                [] ->
                    []

                (Column slots _) :: tail ->
                    getHorizontalSlotsToTest (List.length slots - 1) pid grid

        horizontalWinningCombinationIsPresent =
            if List.length horizontalSlotsToTest < winningStreak then
                False

            else
                wonAgainstSlotsUnderTest horizontalSlotsToTest

        verticalSlotsToTest =
            case targetColumnInList of
                [] ->
                    []

                (Column slots _) :: tail ->
                    getVerticalSlotsToTest pid slots

        verticalWinningCombinationIsPresent =
            if List.length verticalSlotsToTest < winningStreak then
                False

            else
                wonAgainstSlotsUnderTest verticalSlotsToTest

        rightDiagonalSlotsToTest =
            let
                rightDiagonalIndices =
                    case targetColumnInList of
                        [] ->
                            []

                        (Column slots _) :: tail ->
                            getIndexesOfRightDiagonal targetColumnIndex (List.length slots - 1)
            in
            getSlotsToTestOnDiagonal rightDiagonalIndices pid grid

        rightDiagonalWinningCombinationPresent =
            wonAgainstSlotsUnderTest rightDiagonalSlotsToTest

        leftDiagonalSlotsToTest : List SlotUnderTest
        leftDiagonalSlotsToTest =
            let
                leftDiagonalIndices =
                    case targetColumnInList of
                        [] ->
                            []

                        (Column slots _) :: tail ->
                            getIndexesOfLeftDiagonal targetColumnIndex (List.length slots - 1)
            in
            getSlotsToTestOnDiagonal leftDiagonalIndices pid grid

        leftDiagonalWinningCombinationPresent =
            wonAgainstSlotsUnderTest leftDiagonalSlotsToTest
    in
    horizontalWinningCombinationIsPresent || verticalWinningCombinationIsPresent || rightDiagonalWinningCombinationPresent || leftDiagonalWinningCombinationPresent


drawGrid : Players -> GameState -> Grid -> H.Html Msg
drawGrid players gameState grid =
    let
        drawSlot : Slot -> H.Html Msg
        drawSlot slot =
            let
                { color } =
                    getPlayerById slot.filledBy players
            in
            H.div
                [ Attr.class "slot", Attr.style "border-color" color ]
                []

        drawColumn : Column -> H.Html Msg
        drawColumn (Column slots colIndex) =
            H.div
                [ Attr.classList [ ( "column", True ), ( "filled", List.length slots == gridHeight ) ]
                , Attr.title "Click to add a disk"
                , Events.onClick (AddDisk colIndex)
                ]
                (List.map drawSlot slots)

        isTheGameOver =
            case gameState of
                NextMover _ ->
                    False

                Draw ->
                    True

                Winner _ ->
                    True
    in
    H.div
        [ Attr.classList [ ( "grid", True ), ( "game-over", isTheGameOver ) ] ]
        (List.map drawColumn grid)


gameInfo : Players -> GameState -> Grid -> H.Html Msg
gameInfo players gameState _ =
    let
        info =
            case gameState of
                NextMover pid ->
                    H.text ("Next mover: " ++ .name (getPlayerById pid players))

                Winner pid ->
                    H.text (.name (getPlayerById pid players) ++ " won it!")

                Draw ->
                    H.text "Game over, it was a draw!"
    in
    H.div
        [ Attr.class "game-info" ]
        [ info ]



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
        One ->
            Tuple.first players

        Two ->
            Tuple.second players
