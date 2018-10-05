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
    [ { id = 1, x = 0, y = 11 }
    , { id = 2, x = 20, y = 12 }
    , { id = 3, x = 3, y = 13 }
    , { id = 4, x = 4, y = 15 }
    , { id = 5, x = 5, y = 18 }
    ]



-- Calculated values


axisOffsetAmount =
    (min sceneWidth sceneHeight) * axisOffsetRatio


plotRatio =
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


dataToPlotScaleFactor =
    { x = sceneWidth * plotRatio / totalRange.x
    , y = sceneHeight * plotRatio / totalRange.y
    }



-- Frames


frame =
    let
        x =
            sceneWidth * (axisOffsetRatio + paddingRatio)

        y =
            sceneHeigth * (axisOffsetRatio + paddingRatio)

        plotOrigin =
            ( x, y )
    in
        { dataWindow = Frame2d.atCoordinates ( data.min.x, data.min.y )
        , plot =
            { main = Frame2d.atCoordinates plotOrigin
            , xBar =
                Frame2d.atCoordinates plotOrigin
                    |> Frame.translateAlongOwn
                        Frame2d.yAxis
                        -axisOffsetAmount
                    |> Frame.mirroAcross Axis2d.x
            , yBar =
                Frame2d.atCoordinates plotOrigin
                    |> Frame.translateAlongOwn
                        Frame2d.xAxis
                        -axisOffsetAmount
                    |> Frame.rotateBy (degrees 90)
            }
        }



-- Helpers

dataToPlotTransform  point =
  point
  |> Point2d.relativeTo frame.dataWindow
  |> 

toPoint record =
    let
        { x, y } =
            record
    in
        Point2d.fromCoordinates ( x, y )



-- Points in datas coordinate system (global)


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


plotAxisAttributes =
    [ strokeWidth <| px <| inTargetScale axisWidth ]


textAttributes =
    [ fontSize <| px <| inTargetScale 13
    , fontFamily [ "super-sans" ]
    , fontWeight FontWeightLighter
    , fontStretch FontStretchUltraCondensed
    , textRendering TextRenderingOptimizeLegibility
    , textAnchor AnchorMiddle
    , strokeWidth <| px 0
    ]


geometryAttributes =
    [ stroke Color.black
    ]


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
