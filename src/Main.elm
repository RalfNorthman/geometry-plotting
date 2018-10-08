module Main exposing (main)

-- Check getViewport in Browser-Dom for a plot that scales with the window
-- Idea: edges of zoomed-in plot fades to transparency.

import Browser
import Maybe.Extra
import Html exposing (Html)
import Color
import Scale
import Axis
import Statistics
import TypedSvg exposing (svg, g, text_)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Types exposing (..)
import TypedSvg.Core exposing (text)
import Geometry.Svg as Svg
import Axis2d exposing (Axis2d)
import Point2d exposing (Point2d)
import Frame2d exposing (Frame2d)
import Circle2d exposing (Circle2d)
import Vector2d exposing (Vector2d)
import Direction2d exposing (Direction2d)
import LineSegment2d exposing (LineSegment2d)


-- Settings


sceneWidth =
    500


sceneHeight =
    300


axisOffsetRatio =
    0.1


paddingRatio =
    0.15


axisWidth =
    1


circleRadiusBase =
    5


data =
    [ { id = 1, x = 1.32947387, y = 11 }
    , { id = 2, x = 21, y = 0 }
    , { id = 3, x = 3, y = -13 }
    , { id = 4, x = 4, y = -15 }
    , { id = 5, x = 10, y = 18.5 }
    , { id = 6, x = 13, y = 11 }
    , { id = 7, x = 7, y = 4 }
    , { id = 8, x = 15, y = 8 }
    , { id = 9, x = 13, y = 6 }
    ]



-- Calculated values


smallestSceneDimension =
    Basics.min sceneWidth sceneHeight


circleRadius =
    2 + circleRadiusBase * smallestSceneDimension / 700


axisOffset =
    smallestSceneDimension * axisOffsetRatio


padding =
    smallestSceneDimension * paddingRatio


totalOffset =
    axisOffset + padding


getRangeBy accessor listOfRecords =
    listOfRecords
        |> List.map accessor
        |> Statistics.extent
        |> Maybe.withDefault ( 0, 0 )


range =
    { x = getRangeBy .x data
    , y = getRangeBy .y data
    }



-- Scales


scale =
    { x =
        Scale.linear
            ( totalOffset, sceneWidth - padding )
            range.x
    , y =
        Scale.linear
            ( sceneHeight - totalOffset, padding )
            range.y
    }



-- Helpers


convertPoint point =
    let
        ( x, y ) =
            Point2d.coordinates point
    in
        Point2d.fromCoordinates
            ( Scale.convert scale.x x, Scale.convert scale.y y )


dataToPlotTransform point =
    point
        |> convertPoint


toPoint record =
    let
        { x, y } =
            record
    in
        Point2d.fromCoordinates ( x, y )



-- Points


circlePositions =
    data
        |> List.map toPoint
        |> List.map dataToPlotTransform



-- Svg drawing


circlesAttributes =
    [ fill <| Fill Color.lightGreen
    , strokeWidth <| px 1
    , stroke Color.black
    ]


circles =
    circlePositions
        |> List.map
            (Circle2d.withRadius circleRadius)
        |> List.map (Svg.circle2d [])


createAxisAttributes scale_ =
    let
        ( a, b ) =
            Scale.range scale_

        insideTickMin =
            Basics.min a b
                + 25.0
                |> Scale.invert scale_

        insideTickMax =
            Basics.max a b
                - 25.0
                |> Scale.invert scale_

        numberOfInsideTicks =
            abs (a - b) / 100 |> round

        ( min, max ) =
            Scale.domain scale_

        tickList =
            min
                :: max
                :: Statistics.ticks
                    insideTickMin
                    insideTickMax
                    numberOfInsideTicks
    in
        [ Axis.ticks tickList ]


xAxis =
    let
        xAxisAttributes =
            createAxisAttributes scale.x

        downSceneHeightButPadding =
            Vector2d.fromComponents ( 0, sceneHeight - padding )
    in
        Axis.bottom xAxisAttributes scale.x
            |> Svg.translateBy downSceneHeightButPadding


yAxis =
    let
        yAxisAttributes =
            createAxisAttributes scale.y

        rightPadding =
            Vector2d.fromComponents ( padding, 0 )
    in
        Axis.left yAxisAttributes scale.y
            |> Svg.translateBy rightPadding


plotAxisAttributes =
    [ strokeWidth <| px axisWidth ]


geometryAttributes =
    [ stroke Color.black
    ]


rootAttributes =
    [ width <| px sceneWidth
    , height <| px sceneHeight
    ]


scene =
    g []
        [ g circlesAttributes circles
        , g plotAxisAttributes [ xAxis, yAxis ]
        ]



-- Architecture


type alias Model =
    List Int


type Msg
    = None


init : () -> ( Model, Cmd Msg )
init _ =
    ( [], Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html msg
view model =
    svg rootAttributes [ scene ]


subscriptions model =
    Sub.none


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
