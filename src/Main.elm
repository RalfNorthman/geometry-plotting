module Main exposing (main)

import Color
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


-- Check getViewport in Browser-Dom for a plot that scales with the window
-- Transform just the points (for the positions of objects)
-- Use those to position circles, axises and text.


inData =
    [ { id = 1, x = 0, y = 1 }
    , { id = 2, x = 20, y = 2 }
    , { id = 3, x = 3, y = 3 }
    , { id = 4, x = 4, y = 5 }
    , { id = 5, x = 5, y = 8 }
    ]


max list =
    list
        |> List.maximum
        |> Maybe.withDefault 0


min list =
    list
        |> List.minimum
        |> Maybe.withDefault 0


data =
    let
        xs =
            inData |> List.map .x

        ys =
            inData |> List.map .y
    in
        { max = { x = max xs, y = max ys }
        , min = { x = min xs, y = min ys }
        }


range =
    { x = data.max.x - data.min.x
    , y = data.max.y - data.min.y
    }


sceneHeight =
    600


axisOffset =
    1


padding =
    1


scaleFactor =
    let
        totalRange =
            { x =
                range.x + axisOffset + padding
            , y =
                range.y + axisOffset + padding
            }

        calculate =
            (\theRange ->
                sceneHeight
                    / theRange
                    |> round
                    |> toFloat
            )
    in
        { x = calculate totalRange.x, y = calculate totalRange.y }


inTargetScale float =
    float / scaleFactor.y


toPoint record =
    let
        { x, y } =
            record
    in
        Point2d.fromCoordinates ( x, y )


circlesAttributes =
    [ fill <| Fill Color.lightGreen
    , strokeWidth <| px <| inTargetScale 1
    ]


circles =
    inData
        |> List.map toPoint
        |> List.map (Circle2d.withRadius (inTargetScale 3))
        |> List.map (Svg.circle2d [])
        |> g circlesAttributes


plotAxis =
    { x =
        { start =
            Point2d.fromCoordinates
                ( data.min.x, data.min.y - axisOffset )
        , stop =
            Point2d.fromCoordinates
                ( data.max.x, data.min.y - axisOffset )
        }
    , y =
        { start =
            Point2d.fromCoordinates
                ( data.min.x - axisOffset, data.min.y )
        , stop =
            Point2d.fromCoordinates
                ( data.min.x - axisOffset, data.max.y )
        }
    }


xAxis =
    LineSegment2d.from plotAxis.x.start plotAxis.x.stop
        |> Svg.lineSegment2d []


yAxis =
    LineSegment2d.from plotAxis.y.start plotAxis.y.stop
        |> Svg.lineSegment2d []


plotAxisAttributes =
    [ strokeWidth <| px <| inTargetScale 2 ]


bothPlotAxis =
    g plotAxisAttributes [ xAxis, yAxis ]


textAttributes =
    [ fontSize <| px <| inTargetScale 13
    , fontFamily [ "super-sans" ]
    , fontWeight FontWeightLighter
    , fontStretch FontStretchUltraCondensed
    , textRendering TextRenderingOptimizeLegibility
    , strokeWidth <| px 0
    ]


myText posX posY abc =
    text_
        [ x <| px posX
        , y <| px -posY
        ]
        [ text abc
        ]
        |> Svg.mirrorAcross Axis2d.x


testText =
    g textAttributes
        [ myText 0 6 "1234567890" ]


geometryAttributes =
    [ stroke Color.black
    ]


scalePoint =
    let
        x =
            data.min.x - axisOffset - padding

        y =
            data.min.y - axisOffset - padding
    in
        Point2d.fromCoordinates ( x, y )


topLeftPoint =
    Point2d.fromCoordinates ( 0, sceneHeight )


topLeftFrame =
    Frame2d.atPoint topLeftPoint
        |> Frame2d.reverseY


allGeometry =
    g geometryAttributes [ circles, bothPlotAxis, testText ]


geometryPlusText =
    let
        vector =
            Vector2d.withLength (0.1 * sceneHeight) Direction2d.positiveY
    in
        g [] [ allGeometry, testText ]
            |> Svg.scaleAbout scalePoint scaleFactor.y
            |> Svg.relativeTo topLeftFrame
            |> Svg.translateBy vector


rootAttributes =
    [ width <| percent 100
    , height <| px <| 1.1 * sceneHeight
    ]


main =
    svg rootAttributes [ geometryPlusText ]
