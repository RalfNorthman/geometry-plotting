module Main exposing (main)

import Color
import TypedSvg exposing (svg, g, text_)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Types exposing (..)
import TypedSvg.Core exposing (text)
import Geometry.Svg as Svg
import Point2d exposing (Point2d)
import Frame2d exposing (Frame2d)
import Circle2d exposing (Circle2d)
import Vector2d exposing (Vector2d)
import Direction2d exposing (Direction2d)
import LineSegment2d exposing (LineSegment2d)


inData =
    [ { x = 0, y = 1 }
    , { x = 2, y = 2 }
    , { x = 3, y = 3 }
    , { x = 4, y = 5 }
    , { x = 5, y = 8 }
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
    500


axisOffset =
    1


padding =
    1


scaleFactor =
    let
        totalRange =
            range.y + axisOffset + padding
    in
        sceneHeight / totalRange


inTargetScale float =
    float / scaleFactor


toPoint record =
    let
        { x, y } =
            record
    in
        Point2d.fromCoordinates ( x, y )


circlesAttributes =
    [ fill <| Fill Color.lightGreen ]


circles =
    inData
        |> List.map toPoint
        |> List.map (Circle2d.withRadius (inTargetScale 3))
        |> List.map (Svg.circle2d [])
        |> g circlesAttributes


makeAxis startX stopX startY stopY =
    let
        start =
            Point2d.fromCoordinates ( startX, startY )

        stop =
            Point2d.fromCoordinates ( stopX, stopY )
    in
        LineSegment2d.from start stop
            |> Svg.lineSegment2d []


xAxis =
    let
        posY =
            data.min.y - axisOffset
    in
        makeAxis data.min.x data.max.x posY posY


yAxis =
    let
        posX =
            data.min.x - axisOffset
    in
        makeAxis posX posX data.min.y data.max.y


bothAxis =
    g [] [ xAxis, yAxis ]


testText =
    text_
        [ x <| px 0
        , y <| px 8
        , fontSize <| px <| inTargetScale 40
        , fill <| Fill Color.black
        ]
        [ text "Test text."
        ]


geometryAttributes =
    [ strokeWidth <| px <| inTargetScale 1
    , stroke Color.black
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
    g geometryAttributes [ circles, bothAxis, testText ]


geometryPlusText =
    let
        vector =
            Vector2d.withLength (0.1 * sceneHeight) Direction2d.positiveY
    in
        g [] [ allGeometry, testText ]
            |> Svg.scaleAbout scalePoint scaleFactor
            |> Svg.relativeTo topLeftFrame
            |> Svg.translateBy vector


rootAttributes =
    [ width <| percent 100
    , height <| px <| 1.1 * sceneHeight
    ]


main =
    svg rootAttributes [ geometryPlusText ]
