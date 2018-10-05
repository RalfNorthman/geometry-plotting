module Main exposing (main)

-- Check getViewport in Browser-Dom for a plot that scales with the window
-- Transform just the points (for the positions of objects)
-- Use those to position circles, axises and text.
-- Idea: edges of zoomed-in plot fades to transparency.
-- Use Frame2d more for Data and Plot coordinate system

import Browser
import Html exposing (Html)
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


-- Settings


sceneWidth =
    800


sceneHeight =
    600


axisOffsetRatio =
    0.05


paddingRatio =
    0.05


axisWidth =
    1


inData =
    [ { id = 1, x = 0, y = 1 }
    , { id = 2, x = 20, y = 2 }
    , { id = 3, x = 3, y = 3 }
    , { id = 4, x = 4, y = 5 }
    , { id = 5, x = 5, y = 8 }
    ]



-- Frames


frame =
    let
        x =
            sceneWidth * ( axisOffsetRatio, paddingRatio )

        y =
            sceneHeigth * ( axisOffsetRatio, paddingRatio )

        plotOrigin =
            ( x, y )
    in
        { plot = Frame2d.atCoordinates plotOrigin
        , xBar =
            Frame2d.atCoordinates plotOrigin
                |> Frame.translateAlongOwn Frame2d.yAxis -axisOffsetAmount
                |> Frame.mirroAcross Axis2d.x
        , yBar =
            Frame2d.atCoordinates plotOrigin
                |> Frame.translateAlongOwn Frame2d.xAxis -axisOffsetAmount
                |> Frame.rotateBy (degrees 90)
        }



-- Calculated values


axisOffsetAmount =
    (min sceneWidth sceneHeight) * axisOffsetRatio


dataPlotWindowRatio =
    1 - axisOffsetRatio - paddingRatio


data =
    let
        max =
            (\list ->
                list
                    |> List.maximum
                    |> Maybe.withDefault 0
            )

        min =
            (\list ->
                list
                    |> List.minimum
                    |> Maybe.withDefault 0
            )

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


scaleFactor =
    let
        totalRange =
            { x =
                range.x * (1 + axisOffsetRatio + paddingRatio)
            , y =
                range.y * (1 + axisOffsetRatio + paddingRatio)
            }

        getScaleFrom =
            (\theRange dimensionAmount ->
                dimensionAmount
                    / theRange
                    |> round
                    |> toFloat
            )
    in
        { x = getScaleFrom totalRange.x sceneWidth
        , y = getScaleFrom totalRange.y sceneHeight
        }



-- Helpers


inTargetScaleX float =
    float / scaleFactor.x


inTargetScaleY float =
    float / scaleFactor.y


toPoint record =
    let
        { x, y } =
            record
    in
        Point2d.fromCoordinates ( x, y )


dataToPlotCoordinateTransform =
    let
        x =
            data.min.x - axisOffset - padding

        y =
            data.min.y - axisOffset - padding

        bottomLeft =
            Point2d.fromCoordinates ( x, y )

        topLeftPoint =
            Point2d.fromCoordinates ( x, y + sceneHeight )

        topLeftFrame =
            Frame2d.atPoint topLeftPoint
                |> Frame2d.reverseY

        allGeometry =
            g geometryAttributes [ circles, bothPlotAxis, testText ]

        vector =
            Vector2d.withLength (0.1 * sceneHeight) Direction2d.positiveY
    in
        g [] [ allGeometry, testText ]
            |> Svg.scaleAbout bottomLeft scaleFactor.y
            |> Svg.relativeTo topLeftFrame
            |> Svg.translateBy vector



-- Points in datas coordinate system


circlePositions =
    inData
        |> List.map toPoint


plotAxisPoints =
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



-- Geometry


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


bothPlotAxis =
    let
        xAxis =
            LineSegment2d.from
                plotAxisPoints.x.start
                plotAxisPoints.x.stop
                |> Svg.lineSegment2d []

        yAxis =
            LineSegment2d.from
                plotAxisPoints.y.start
                plotAxisPoints.y.stop
                |> Svg.lineSegment2d []

        plotAxisAttributes =
            [ strokeWidth <| px <| inTargetScale axisWidth ]
    in
        g plotAxisAttributes [ xAxis, yAxis ]


textAttributes =
    [ fontSize <| px <| inTargetScale 13
    , fontFamily [ "super-sans" ]
    , fontWeight FontWeightLighter
    , fontStretch FontStretchUltraCondensed
    , textRendering TextRenderingOptimizeLegibility
    , textAnchor AnchorMiddle
    , strokeWidth <| px 0
    ]


myText posX posY adjustY abc =
    text_
        [ x <| px posX
        , y <| px -posY
        , dy <| px <| inTargetScale -adjustY
        ]
        [ text abc
        ]
        |> Svg.mirrorAcross Axis2d.x


testText =
    g textAttributes
        [ myText 0 0 -20 "1234567890" ]


geometryAttributes =
    [ stroke Color.black
    ]


geometryPlusText =
    let
        x =
            data.min.x - axisOffset - padding

        y =
            data.min.y - axisOffset - padding

        bottomLeft =
            Point2d.fromCoordinates ( x, y )

        topLeftPoint =
            Point2d.fromCoordinates ( x, y + sceneHeight )

        topLeftFrame =
            Frame2d.atPoint topLeftPoint
                |> Frame2d.reverseY

        allGeometry =
            g geometryAttributes [ circles, bothPlotAxis, testText ]

        vector =
            Vector2d.withLength (0.1 * sceneHeight) Direction2d.positiveY
    in
        g [] [ allGeometry, testText ]
            |> Svg.scaleAbout bottomLeft scaleFactor.y
            |> Svg.relativeTo topLeftFrame
            |> Svg.translateBy vector


rootAttributes =
    [ width <| percent 100
    , height <| px <| 1.1 * sceneHeight
    ]


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
    svg rootAttributes [ geometryPlusText ]


subscriptions model =
    Sub.none


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
