module Main exposing (main)

import Color
import TypedSvg exposing (svg, g)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Types exposing (..)
import Geometry.Svg as Svg
import Point2d exposing (Point2d)
import Circle2d exposing (Circle2d)
import Direction2d exposing (Direction2d)
import LineSegment2d exposing (LineSegment2d)


data =
    [ { x = 0, y = 1 }
    , { x = 2, y = 2 }
    , { x = 3, y = 3 }
    , { x = 4, y = 5 }
    , { x = 5, y = 8 }
    ]


toPoint record =
    let
        { x, y } =
            record
    in
        Point2d.fromCoordinates ( x, y )


circlesAttributes =
    []


circles =
    data
        |> List.map toPoint
        |> List.map (Point2d.translateIn (Direction2d.fromAngle 45) 2)
        |> List.map (Point2d.scaleAbout Point2d.origin 20)
        |> List.map (Circle2d.withRadius 1)
        |> List.map (Svg.circle2d [])
        |> g circlesAttributes


axisOffset =
    -2


xAxis =
    let
        start =
            Point2.fromCoordinates ( 0, axisOffset )

        stop =
            Point2.fromCoordinates ( 5, axisOffset )
    in
        LineSegment2d.from start stop


yAxis =
    let
        start =
            Point2.fromCoordinates ( axisOffset, 1 )

        stop =
            Point2.fromCoordinates ( axisOffset, 8 )
    in
        LineSegment2d.from start stop


rootAttributes =
    [ width <| percent 100
    , height <| px 500
    ]


main =
    svg rootAttributes [ circles ]
