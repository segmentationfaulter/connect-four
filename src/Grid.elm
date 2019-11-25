module Grid exposing (..)

import Main exposing (Player)


gridHeight =
    6


gridWidth =
    7


type alias ColumnIndex =
    Int


type alias RowIndex =
    Int


type alias Slot =
    { filledBy : Player
    , rowIndex : RowIndex
    }


type alias SlotUnderTest =
    { filledBy : Player
    , index : Int
    }


type Column
    = Column (List Slot) ColumnIndex


type alias Grid =
    List Column


initialGrid : Grid
initialGrid =
    List.repeat gridWidth [] |> List.indexedMap (\colIndex column -> Column column colIndex)


addDisk : Player -> ColumnIndex -> Grid -> Grid
addDisk player targetIndex grid =
    let
        mapper : Column -> Column
        mapper (Column slots colIndex) =
            let
                rowIndex =
                    List.length slots

                newSlot =
                    Slot player rowIndex
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

getIndexesOfRightDiagonal : ColumnIndex -> RowIndex -> List (ColumnIndex, RowIndex)    
getIndexesOfRightDiagonal targetColumnIndex targetRowIndex =
    let
        upperHalfOfIndexes : ColumnIndex -> RowIndex -> List (ColumnIndex, RowIndex) -> List (ColumnIndex, RowIndex)
        upperHalfOfIndexes colIndex rowIndex accumulator =
            if (colIndex < gridWidth && rowIndex < gridHeight) then
                upperHalfOfIndexes (colIndex + 1) (rowIndex + 1) (accumulator ++ [(colIndex, rowIndex)])
            else
                accumulator
        lowerHalfOfIndexes : ColumnIndex -> RowIndex -> List (ColumnIndex, RowIndex) -> List (ColumnIndex, RowIndex)
        lowerHalfOfIndexes colIndex rowIndex accumulator =
            if (colIndex > -1 && rowIndex > -1) then
                lowerHalfOfIndexes (colIndex - 1) (rowIndex - 1) ((colIndex, rowIndex)::accumulator)
            else
                accumulator
    in
        lowerHalfOfIndexes targetColumnIndex targetRowIndex [] ++ upperHalfOfIndexes (targetColumnIndex + 1) (targetRowIndex + 1) []


-- Left diagonal is the one which link bottom right to top left

getIndexesOfLeftDiagonal : ColumnIndex -> RowIndex -> List (ColumnIndex, RowIndex)
getIndexesOfLeftDiagonal targetColumnIndex targetRowIndex =
    let
        upperHalfOfIndexes : ColumnIndex -> RowIndex -> List (ColumnIndex, RowIndex) -> List (ColumnIndex, RowIndex)
        upperHalfOfIndexes colIndex rowIndex accumulator =
            if (colIndex > -1 && rowIndex < gridHeight) then
                upperHalfOfIndexes (colIndex - 1) (rowIndex + 1) (accumulator ++ [(colIndex, rowIndex)])
            else
                accumulator

        lowerHalfOfIndexes : ColumnIndex -> RowIndex -> List (ColumnIndex, RowIndex) -> List (ColumnIndex, RowIndex)
        lowerHalfOfIndexes colIndex rowIndex accumulator =
            if (colIndex < gridWidth && rowIndex > -1) then
                lowerHalfOfIndexes (colIndex + 1) (rowIndex - 1) ((colIndex, rowIndex)::accumulator)
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
