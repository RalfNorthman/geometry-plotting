module Main exposing (main)

-- Check getViewport in Browser-Dom for a plot that scales with the window
-- Transform just the points (for the positions of objects)
-- Use those to position circles, axises and text.
-- Idea: edges of zoomed-in plot fades to transparency.
-- Use Frame2d more for Data and Plot coordinate system

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
    400


axisOffsetRatio =
    0.1


paddingRatio =
    0.1


axisWidth =
    1


data2 =
    [ { id = 1, x = 0, y = 0 }
    , { id = 3, x = 0, y = 1 }
    , { id = 4, x = 1, y = 0 }
    , { id = 5, x = 1, y = 1 }
    , { id = 2, x = 0.5, y = 0.5 }
    , { id = 6, x = 0.75, y = 0.25 }
    ]


data =
    [ { id = 1, x = 1.32947387, y = 11 }
    , { id = 3, x = 3, y = -13 }
    , { id = 4, x = 4, y = -15 }
    , { id = 5, x = 10, y = 18.5 }
    , { id = 2, x = 20, y = 0 }
    , { id = 6, x = 13, y = 11 }
    , { id = 7, x = 7, y = 4 }
    , { id = 8, x = 15, y = 8 }
    , { id = 9, x = 13, y = 6 }
    ]



-- Calculated values


axisOffset =
    (Basics.min sceneWidth sceneHeight) * axisOffsetRatio


padding =
    (Basics.min sceneWidth sceneHeight) * paddingRatio


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
    ]


circles =
    circlePositions
        |> List.map
            (Circle2d.withRadius 5)
        |> List.map (Svg.circle2d [])


createAxisAttributes list =
    let
        min =
            List.minimum list

        max =
            List.maximum list

        quantiles =
            List.map
                (\q -> Statistics.quantile q list)
                [ 0.25, 0.5, 0.75 ]

        tickList =
            min
                :: (max :: quantiles)
                |> Maybe.Extra.values
    in
        [ Axis.ticks tickList ]


xAxis =
    let
        xs =
            List.map .x data

        xAxisAttributes =
            createAxisAttributes xs
    in
        Axis.bottom xAxisAttributes scale.x
            |> Svg.translateBy
                (Vector2d.fromComponents ( 0, sceneHeight - padding ))


yAxis =
    let
        ys =
            List.map .y data

        yAxisAttributes =
            createAxisAttributes ys
    in
        Axis.left yAxisAttributes scale.y
            |> Svg.translateBy
                (Vector2d.fromComponents ( padding, 0 ))


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
