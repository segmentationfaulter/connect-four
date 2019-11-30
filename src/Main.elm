module Main exposing (main)

import Browser
import Html as H
import Html.Attributes as Attr
import Html.Events as Events
import String
import Time
import Tuple


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- Model


type Model
    = ShowingStartScreen StartScreen
    | ShowingGameBoard Players GameState Grid


type StartScreen
    = ShowingDefaults Players
    | ShowingFormToChangeDefaults Players


type alias Players =
    ( Player, Player )


type PlayerID
    = One
    | Two


type GameState
    = CurrentMover PlayerID
    | Winner PlayerID
    | Draw


type alias Player =
    { name : String
    , color : String
    , id : PlayerID
    , timeElapsed : Int
    , winsCount : Int
    }


type alias ColumnIndex =
    Int


type alias RowIndex =
    Int


type alias Slot =
    { filledBy : PlayerID
    , rowIndex : RowIndex
    }


type alias SlotsUnderTest =
    List Int


type Column
    = Column (List Slot) ColumnIndex


type alias Grid =
    List Column


gridHeight : Int
gridHeight =
    6


gridWidth : Int
gridWidth =
    7


winningStreak : Int
winningStreak =
    4


init : () -> ( Model, Cmd Msg )
init () =
    ( ShowingStartScreen (ShowingDefaults ( { name = "Player1", color = "#FF0000", id = One, timeElapsed = 0, winsCount = 0 }, { name = "Player2", color = "#0000FF", id = Two, timeElapsed = 0, winsCount = 0 } )), Cmd.none )


initialGrid : Grid
initialGrid =
    List.repeat gridWidth [] |> List.indexedMap (\colIndex column -> Column column colIndex)



-- Subscription


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        ShowingGameBoard _ gameState _ ->
            case gameState of
                CurrentMover _ ->
                    Time.every 1000 Tick

                _ ->
                    Sub.none

        ShowingStartScreen _ ->
            Sub.none



-- Update


type Msg
    = ProceedToChangeDefaults
    | PlayerNameChanged PlayerID String
    | PlayerColorChanged PlayerID String
    | ProceedToPlay
    | AddDisk ColumnIndex
    | Tick Time.Posix
    | PlayAgain
    | GoToStartScreen


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProceedToChangeDefaults ->
            case model of
                ShowingStartScreen startScreen ->
                    case startScreen of
                        ShowingFormToChangeDefaults _ ->
                            ( model, Cmd.none )

                        ShowingDefaults players ->
                            ( ShowingStartScreen (ShowingFormToChangeDefaults players), Cmd.none )

                ShowingGameBoard _ _ _ ->
                    ( model, Cmd.none )

        PlayerNameChanged id name ->
            case id of
                One ->
                    case model of
                        ShowingStartScreen startScreen ->
                            case startScreen of
                                ShowingDefaults _ ->
                                    ( model, Cmd.none )

                                ShowingFormToChangeDefaults ( p1, p2 ) ->
                                    ( ShowingStartScreen (ShowingFormToChangeDefaults ( { p1 | name = name }, p2 )), Cmd.none )

                        ShowingGameBoard _ _ _ ->
                            ( model, Cmd.none )

                Two ->
                    case model of
                        ShowingStartScreen startScreen ->
                            case startScreen of
                                ShowingDefaults _ ->
                                    ( model, Cmd.none )

                                ShowingFormToChangeDefaults ( p1, p2 ) ->
                                    ( ShowingStartScreen (ShowingFormToChangeDefaults ( p1, { p2 | name = name } )), Cmd.none )

                        ShowingGameBoard _ _ _ ->
                            ( model, Cmd.none )

        PlayerColorChanged id color ->
            case id of
                One ->
                    case model of
                        ShowingStartScreen startScreen ->
                            case startScreen of
                                ShowingDefaults _ ->
                                    ( model, Cmd.none )

                                ShowingFormToChangeDefaults ( p1, p2 ) ->
                                    ( ShowingStartScreen (ShowingFormToChangeDefaults ( { p1 | color = color }, p2 )), Cmd.none )

                        ShowingGameBoard _ _ _ ->
                            ( model, Cmd.none )

                Two ->
                    case model of
                        ShowingStartScreen startScreen ->
                            case startScreen of
                                ShowingDefaults _ ->
                                    ( model, Cmd.none )

                                ShowingFormToChangeDefaults ( p1, p2 ) ->
                                    ( ShowingStartScreen (ShowingFormToChangeDefaults ( p1, { p2 | color = color } )), Cmd.none )

                        ShowingGameBoard _ _ _ ->
                            ( model, Cmd.none )

        ProceedToPlay ->
            case model of
                ShowingStartScreen startScreen ->
                    case startScreen of
                        ShowingDefaults players ->
                            ( ShowingGameBoard players (CurrentMover One) initialGrid, Cmd.none )

                        ShowingFormToChangeDefaults players ->
                            ( ShowingGameBoard players (CurrentMover One) initialGrid, Cmd.none )

                ShowingGameBoard _ _ _ ->
                    ( model, Cmd.none )

        AddDisk colIndex ->
            case model of
                ShowingStartScreen _ ->
                    ( model, Cmd.none )

                ShowingGameBoard players gameState currentGrid ->
                    case gameState of
                        CurrentMover pid ->
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
                                ( ShowingGameBoard (incrementWinsCount pid players) (Winner pid) updatedGrid, Cmd.none )

                            else if not moverWonIt && slotsVacantInGrid then
                                ( ShowingGameBoard players (CurrentMover nextMover) updatedGrid, Cmd.none )

                            else
                                ( ShowingGameBoard players Draw updatedGrid, Cmd.none )

                        Draw ->
                            ( model, Cmd.none )

                        Winner _ ->
                            ( model, Cmd.none )

        Tick _ ->
            case model of
                ShowingStartScreen _ ->
                    ( model, Cmd.none )

                ShowingGameBoard ( p1, p2 ) gameState grid ->
                    case gameState of
                        CurrentMover pid ->
                            case pid of
                                One ->
                                    ( ShowingGameBoard ( { p1 | timeElapsed = p1.timeElapsed + 1 }, p2 ) gameState grid, Cmd.none )

                                Two ->
                                    ( ShowingGameBoard ( p1, { p2 | timeElapsed = p2.timeElapsed + 1 } ) gameState grid, Cmd.none )

                        Winner _ ->
                            ( model, Cmd.none )

                        Draw ->
                            ( model, Cmd.none )

        PlayAgain ->
            case model of
                ShowingStartScreen _ ->
                    ( model, Cmd.none )

                ShowingGameBoard players gameState _ ->
                    let
                        playersAfterTimerReset =
                            let
                                resetter : Player -> Player
                                resetter player =
                                    { player | timeElapsed = 0 }
                            in
                            Tuple.mapBoth resetter resetter players
                    in
                    case gameState of
                        CurrentMover _ ->
                            ( model, Cmd.none )

                        Draw ->
                            ( ShowingGameBoard playersAfterTimerReset (CurrentMover One) initialGrid, Cmd.none )

                        Winner pid ->
                            case pid of
                                One ->
                                    ( ShowingGameBoard playersAfterTimerReset (CurrentMover Two) initialGrid, Cmd.none )

                                Two ->
                                    ( ShowingGameBoard playersAfterTimerReset (CurrentMover One) initialGrid, Cmd.none )

        GoToStartScreen ->
            case model of
                ShowingGameBoard _ _ _ ->
                    init ()

                ShowingStartScreen _ ->
                    ( model, Cmd.none )



-- View


view : Model -> H.Html Msg
view model =
    case model of
        ShowingStartScreen startScreen ->
            case startScreen of
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


getHorizontalSlotsToTest : RowIndex -> PlayerID -> Grid -> SlotsUnderTest
getHorizontalSlotsToTest rowIndex pid grid =
    let
        mapper : Column -> SlotsUnderTest
        mapper (Column slots colIndex) =
            slots
                |> List.filter (\slot -> slot.rowIndex == rowIndex && slot.filledBy == pid)
                |> List.map (\_ -> colIndex)
    in
    grid
        |> List.map mapper
        |> List.concat


getVerticalSlotsToTest : PlayerID -> List Slot -> SlotsUnderTest
getVerticalSlotsToTest pid slots =
    slots
        |> List.filter (\slot -> slot.filledBy == pid)
        |> List.map (\slot -> slot.rowIndex)



-- Right diagonal is the one which links bottom left to top right


getIndicesOfRightDiagonal : ColumnIndex -> RowIndex -> List ( ColumnIndex, RowIndex )
getIndicesOfRightDiagonal targetColumnIndex targetRowIndex =
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


getIndicesOfLeftDiagonal : ColumnIndex -> RowIndex -> List ( ColumnIndex, RowIndex )
getIndicesOfLeftDiagonal targetColumnIndex targetRowIndex =
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


getSlotsToTestOnDiagonal : List ( ColumnIndex, RowIndex ) -> PlayerID -> Grid -> SlotsUnderTest
getSlotsToTestOnDiagonal indices pid grid =
    let
        ( columnIndices, rowIndices ) =
            List.unzip indices

        targetColumns =
            List.filter (\(Column _ colIndex) -> List.member colIndex columnIndices) grid

        mapper : Column -> RowIndex -> SlotsUnderTest
        mapper (Column slots colIndex) rowIndex =
            slots
                |> List.filter (\slot -> slot.rowIndex == rowIndex && slot.filledBy == pid)
                |> List.map (\_ -> colIndex)
    in
    List.map2 mapper targetColumns rowIndices
        |> List.concat


wonAgainstSlotsUnderTest : SlotsUnderTest -> Bool
wonAgainstSlotsUnderTest slots =
    let
        helper : SlotsUnderTest -> { streak : Int, lastSlotIndex : Maybe Int } -> Int
        helper slots_ { streak, lastSlotIndex } =
            case slots_ of
                headElementIndex :: rest ->
                    case lastSlotIndex of
                        Just index ->
                            if abs (headElementIndex - index) == 1 then
                                helper rest { streak = streak + 1, lastSlotIndex = Just headElementIndex }

                            else
                                helper rest { streak = 1, lastSlotIndex = Just headElementIndex }

                        Nothing ->
                            helper rest { streak = 1, lastSlotIndex = Just headElementIndex }

                [] ->
                    streak
    in
    if helper slots { streak = 0, lastSlotIndex = Nothing } > 3 then
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

                (Column slots _) :: _ ->
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

                (Column slots _) :: _ ->
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

                        (Column slots _) :: _ ->
                            getIndicesOfRightDiagonal targetColumnIndex (List.length slots - 1)
            in
            getSlotsToTestOnDiagonal rightDiagonalIndices pid grid

        rightDiagonalWinningCombinationPresent =
            wonAgainstSlotsUnderTest rightDiagonalSlotsToTest

        leftDiagonalSlotsToTest : SlotsUnderTest
        leftDiagonalSlotsToTest =
            let
                leftDiagonalIndices =
                    case targetColumnInList of
                        [] ->
                            []

                        (Column slots _) :: _ ->
                            getIndicesOfLeftDiagonal targetColumnIndex (List.length slots - 1)
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
                CurrentMover _ ->
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
        stats : Players -> H.Html Msg
        stats ( p1, p2 ) =
            let
                playerStats : Player -> H.Html Msg
                playerStats player =
                    H.div
                        []
                        [ H.h3 [] [ H.text player.name ]
                        , H.div [] [ H.text ("Time Elapsed: " ++ String.fromInt player.timeElapsed) ]
                        , H.div [] [ H.text ("Wins: " ++ String.fromInt player.winsCount) ]
                        ]
            in
            H.div
                [ Attr.class "player-stats" ]
                [ playerStats p1
                , playerStats p2
                ]

        info =
            case gameState of
                CurrentMover pid ->
                    H.text ("Current mover: " ++ .name (getPlayerById pid players))

                Winner pid ->
                    H.text (.name (getPlayerById pid players) ++ " won it!")

                Draw ->
                    H.text "Game over, it was a draw!"

        controlsView =
            let
                controls =
                    H.div
                        [ Attr.class "reset-controls" ]
                        [ H.button [ Attr.type_ "button", Events.onClick PlayAgain ] [ H.text "Play Again" ]
                        , H.button [ Attr.type_ "button", Events.onClick GoToStartScreen ] [ H.text "Go to Start Screen" ]
                        ]
            in
            case gameState of
                CurrentMover _ ->
                    H.text ""

                Draw ->
                    controls

                Winner _ ->
                    controls
    in
    H.div
        [ Attr.class "game-info" ]
        [ info
        , stats players
        , controlsView
        ]



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


incrementWinsCount : PlayerID -> Players -> Players
incrementWinsCount pid ( p1, p2 ) =
    case pid of
        One ->
            ( { p1 | winsCount = p1.winsCount + 1 }, p2 )

        Two ->
            ( p1, { p2 | winsCount = p2.winsCount + 1 } )
