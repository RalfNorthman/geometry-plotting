module Main exposing (main)

-- Check getViewport in Browser-Dom for a plot that scales with the window
-- Transform just the points (for the positions of objects)
-- Use those to position circles, axises and text.
-- Idea: edges of zoomed-in plot fades to transparency.
-- Use Frame2d more for Data and Plot coordinate system

import Browser
import Html exposing (Html)
import Color
import Scale
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
    400


axisOffsetRatio =
    0.1


paddingRatio =
    0.1


axisWidth =
    1


inData =
    [ { id = 1, x = 0, y = 0 }
    , { id = 3, x = 0, y = 1 }
    , { id = 4, x = 1, y = 0 }
    , { id = 5, x = 1, y = 1 }
    , { id = 2, x = 0.5, y = 0.5 }
    , { id = 6, x = 0.75, y = 0.25 }
    ]


inData2 =
    [ { id = 1, x = 0, y = 11 }
    , { id = 3, x = 3, y = -13 }
    , { id = 4, x = 4, y = -15 }
    , { id = 5, x = 10, y = 18 }
    , { id = 2, x = 20, y = 12 }
    ]



-- Calculated values


axisOffset =
    (Basics.min sceneWidth sceneHeight) * axisOffsetRatio


padding =
    (Basics.min sceneWidth sceneHeight) * paddingRatio


totalOffset =
    axisOffset + padding


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



-- Frames


frame =
    let
        plotOrigin =
            ( totalOffset, totalOffset )
    in
        { dataWindow = Frame2d.atCoordinates ( data.min.x, data.min.y )
        , plot =
            { main = Frame2d.atCoordinates plotOrigin
            , xBar =
                Frame2d.atCoordinates plotOrigin
                    |> Frame2d.translateAlongOwn
                        Frame2d.yAxis
                        -axisOffset
                    |> Frame2d.reverseY
            , yBar =
                Frame2d.atCoordinates plotOrigin
                    |> Frame2d.translateAlongOwn
                        Frame2d.xAxis
                        -axisOffset
                    |> Frame2d.rotateBy (degrees 90)
            }
        , finalFlip =
            Frame2d.atCoordinates ( 0, sceneHeight )
                |> Frame2d.reverseY
        }



-- Scales


scale =
    { x =
        Scale.linear
            ( totalOffset, sceneWidth - padding )
            ( data.min.x, data.max.x )
    , y =
        Scale.linear
            ( totalOffset, sceneHeight - padding )
            ( data.min.y, data.max.y )
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
        |> Point2d.placeIn frame.finalFlip


toPoint record =
    let
        { x, y } =
            record
    in
        Point2d.fromCoordinates ( x, y )



-- Points


circlePositions =
    inData
        |> List.map toPoint
        |> List.map dataToPlotTransform


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



-- Svg drawing


circlesAttributes =
    [ fill <| Fill Color.lightGreen
    , strokeWidth <| px 1
    ]


circles =
    circlePositions
        |> List.map
            (Circle2d.withRadius 10)
        |> List.map (Svg.circle2d [])


plotAxisAttributes =
    [ strokeWidth <| px axisWidth ]


textAttributes =
    [ fontSize <| px 13
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
    [ width <| px sceneWidth
    , height <| px sceneHeight
    ]


scene =
    g geometryAttributes
        [ g circlesAttributes circles ]



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
