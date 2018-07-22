module Main exposing (..)

import Html exposing (Html, Attribute, div, h1, text, span, table, tr, td)
import Html.Attributes exposing (attribute, align, style)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Random
import Maybe
import Time


---- MODEL ----


type alias Model =
    { scene : Scene
    , positionList : List Position
    , cellMatrix : CellMatrix
    , neighborCountMatrix : NeighborCountMatrix
    , clockPeriodInMillisecond : Time.Time
    , matrixWidth : Int
    , matrixHeight : Int
    , initialDensity : Int
    }

init : ( Model, Cmd Msg )
init =
    let
        width = 70
        height = 30
        clock = 100
        density = 40
    in
        ( { scene = Initializing
          , positionList = initPositionList width height
          , cellMatrix = Array.repeat height <| Array.repeat width Dead
          , neighborCountMatrix = initNeighborCountMatrix width height
          , clockPeriodInMillisecond = clock * Time.millisecond
          , matrixWidth = width
          , matrixHeight = height
          , initialDensity = density
          }
        , Random.generate InitializeCellMatrix <| initCellMatrix width height density)

type Scene
    = Initializing
    | Playing
    | Pausing

type alias Position =
    { x : Int
    , y : Int
    }

initPositionList : Int -> Int -> List Position
initPositionList width height =
    Array.initialize (width * height) (\n -> {x = n % width, y = n // width})
        |> Array.toList

type Cell
    = Dead
    | Alive

type alias CellMatrix =
    Array (Array Cell)

initCellMatrix : Int -> Int -> Int -> Random.Generator CellMatrix
initCellMatrix width height density =
    Random.map (\rand -> if rand < density then Alive else Dead) (Random.int 0 99)
        |> Random.list width
        |> Random.map Array.fromList
        |> Random.list height
        |> Random.map Array.fromList

type alias NeighborCount
    = Int

type alias NeighborCountMatrix =
    Array (Array NeighborCount)

initNeighborCountMatrix : Int -> Int -> NeighborCountMatrix
initNeighborCountMatrix width height =
    Array.repeat width 0
        |> Array.repeat height


---- UPDATE ----


type Msg
    = InitializeCellMatrix CellMatrix
    | UpdateCellMatrix
    | Start
    | Pause
    | Reset

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitializeCellMatrix cells ->
            ( { model | scene = Pausing, cellMatrix = cells }
            , Cmd.none
            )
        UpdateCellMatrix ->
            let
                newNeighborCountMatrix = countNeighbors model.positionList model.matrixWidth model.matrixHeight model.cellMatrix
            in
                ( { model | neighborCountMatrix = newNeighborCountMatrix, cellMatrix = createCellMatrix model.cellMatrix newNeighborCountMatrix }
                , Cmd.none
                )
        Start ->
            ( { model | scene = Playing }
            , Cmd.none
            )
        Pause ->
            ( { model | scene = Pausing }
            , Cmd.none
            )
        Reset ->
            ( { model | scene = Initializing }
            , Random.generate InitializeCellMatrix <| initCellMatrix model.matrixWidth model.matrixHeight model.initialDensity
            )

countNeighbors : List Position -> Int -> Int -> CellMatrix -> NeighborCountMatrix
countNeighbors positionList width hegiht cellMatrix =
    initNeighborCountMatrix width hegiht
        |> affectNeighborCountMatrixMultiply (List.filterMap (isAlive cellMatrix) positionList)

isAlive : CellMatrix -> Position -> Maybe Position
isAlive cellMatrix position =
    let
        cell =
            Array.get position.y cellMatrix
                |> Maybe.withDefault Array.empty
                |> Array.get position.x
                |> Maybe.withDefault Dead
    in
        case cell of
            Alive ->
                Just position
            Dead ->
                Nothing

type DirectionScalar
    = Positive
    | Zero
    | Negative

type alias DirectionVector =
    { x : DirectionScalar
    , y : DirectionScalar
    }

affectNeighborCountMatrixMultiply : List Position -> NeighborCountMatrix -> NeighborCountMatrix
affectNeighborCountMatrixMultiply positionList neighborCountMatrix =
    let
        position = List.head positionList |> Maybe.withDefault { x = 0, y = 0}
        directionUpper = { x = Zero, y = Negative }
        directionLower = { x = Zero, y = Positive }
        directionLeft = { x = Negative, y = Zero }
        directionRight = { x = Positive, y = Zero }
        directionUpperLeft = { x = Negative, y = Negative }
        directionUpperRight = { x = Positive, y = Negative }
        directionLowerLeft = { x = Negative, y = Positive }
        directionLowerRight = { x = Positive, y = Positive }
        upperNeighborCount = calcDirectedNeighborCount position directionUpper neighborCountMatrix
        lowerNeighborCount = calcDirectedNeighborCount position directionLower neighborCountMatrix
        leftNeighborCount = calcDirectedNeighborCount position directionLeft neighborCountMatrix
        rightNeighborCount = calcDirectedNeighborCount position directionRight neighborCountMatrix
        upperLeftNeighborCount = calcDirectedNeighborCount position directionUpperLeft neighborCountMatrix
        upperRightNeighborCount = calcDirectedNeighborCount position directionUpperRight neighborCountMatrix
        lowerLeftNeighborCount = calcDirectedNeighborCount position directionLowerLeft neighborCountMatrix
        lowerRightNeighborCount = calcDirectedNeighborCount position directionLowerRight neighborCountMatrix
    in
        if List.isEmpty positionList then
            neighborCountMatrix
        else
            neighborCountMatrix
                |> affectNeighborCountMatrix position directionUpper upperNeighborCount
                |> affectNeighborCountMatrix position directionLower lowerNeighborCount
                |> affectNeighborCountMatrix position directionLeft leftNeighborCount
                |> affectNeighborCountMatrix position directionRight rightNeighborCount
                |> affectNeighborCountMatrix position directionUpperLeft upperLeftNeighborCount
                |> affectNeighborCountMatrix position directionUpperRight upperRightNeighborCount
                |> affectNeighborCountMatrix position directionLowerLeft lowerLeftNeighborCount
                |> affectNeighborCountMatrix position directionLowerRight lowerRightNeighborCount
                |> affectNeighborCountMatrixMultiply (List.tail positionList |> Maybe.withDefault [])

affectNeighborCountMatrix : Position -> DirectionVector -> Maybe NeighborCount -> NeighborCountMatrix -> NeighborCountMatrix
affectNeighborCountMatrix position direction maybeNeighborCount neighborCountMatrix =
    let
        neighborCount = (Maybe.withDefault -1 maybeNeighborCount) + 1
    in
        if maybeNeighborCount == Nothing then
            neighborCountMatrix
        else
            neighborCountMatrix
                |> Array.get (position.y + (toInt direction.y))
                |> Maybe.withDefault Array.empty
                |> Array.set (position.x + (toInt direction.x)) neighborCount
                |> (\array -> Array.set (position.y + (toInt direction.y)) array neighborCountMatrix)

calcDirectedNeighborCount : Position -> DirectionVector -> NeighborCountMatrix -> Maybe NeighborCount
calcDirectedNeighborCount position direction neighborCountMatrix =
    neighborCountMatrix
        |> Array.get (position.y + (toInt direction.y))
        |> Maybe.andThen (Array.get (position.x + (toInt direction.x)))

toInt : DirectionScalar -> Int
toInt direction =
    case direction of
        Positive ->
            1
        Zero ->
            0
        Negative ->
            -1

createCellMatrix : CellMatrix -> NeighborCountMatrix -> CellMatrix
createCellMatrix cellMatrix neighborCountMatrix =
    Array.indexedMap
        ( \i -> \array -> createCellArray (Array.get i cellMatrix |> Maybe.withDefault Array.empty) array )
        neighborCountMatrix

createCellArray :  Array Cell -> Array NeighborCount -> Array Cell
createCellArray cellArray neighborCountArray =
    Array.indexedMap
        ( \i -> \neighborCount -> createCell (Array.get i cellArray |> Maybe.withDefault Dead) neighborCount )
        neighborCountArray

createCell : Cell -> NeighborCount -> Cell
createCell cell neighborCount =
    case ( cell, neighborCount) of
        (Alive, 2) ->
            Alive
        (Alive, 3) ->
            Alive
        (Dead, 3) ->
            Alive
        _ ->
            Dead


---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 []
            [ text "GAME OF LIFE |"
            , span [ onClick Start, generateSpanStyle model.scene Start ] [ text "START" ]
            , span [ onClick Pause, generateSpanStyle model.scene Pause ] [ text "PAUSE" ]
            , span [ onClick Reset, generateSpanStyle model.scene Reset ] [ text "RESET" ]
            ]
        , generateTable cellStyle model.cellMatrix
        ]

generateSpanStyle : Scene -> Msg -> Attribute Msg
generateSpanStyle scene msg =
    let
        display =
            case (scene, msg) of
                (Playing, Pause) ->
                    []
                (Pausing, Start) ->
                    []
                (_, Reset) ->
                    []
                _ ->
                    [ ("display", "none")]
    in
        [ ("font-size", "26px")
        , ("color", "oive")
        , ("backgroundColor", "lightgreen")
        , ("padding", "4px")
        , ("margin-left", "0.5em")
        ]
        |> List.append display
        |> style

generateTable : CellStyle -> CellMatrix -> Html Msg
generateTable cellStyle cellMatrix =
    cellMatrix
        |> Array.map (generateTableRow cellStyle)
        |> Array.toList
        |> table [ align "center" ]

generateTableRow : CellStyle -> Array Cell -> Html Msg
generateTableRow cellStyle cellArray =
    cellArray
        |> Array.map (generateTableData cellStyle)
        |> Array.toList
        |> tr []

generateTableData : CellStyle -> Cell -> Html Msg
generateTableData cellStyle cell =
    case cell of
        Alive ->
            td [ cellStyle.alive ] []
        Dead ->
            td [ cellStyle.dead ] []

type alias CellStyle = { alive : Attribute Msg, dead : Attribute Msg }
cellStyle : CellStyle
cellStyle =
    { alive =
        style
            [ ("backgroundColor", "lightgreen")
            , ("height", "1em")
            , ("width", "1em")
            ]
    , dead =
        style
            [ ("backgroundColor", "white")
            , ("height", "1em")
            , ("width", "1em")
            ]
    }


---- SUBSCRIPTION ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.scene of
        Playing ->
            Time.every model.clockPeriodInMillisecond (always UpdateCellMatrix)
        _ ->
            Sub.none


---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
