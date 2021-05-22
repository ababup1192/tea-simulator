module Main exposing (main)

import Browser
import Html exposing (Html)
import Json.Decode as JD
import Process
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick, stopPropagationOn)
import Task


animationTime =
    2000


type ModelBlock modelType
    = ModelBlock modelType


type LabelType
    = PlusOneLabel
    | MinusOneLabel


labelType2Text : LabelType -> String
labelType2Text labelType =
    case labelType of
        PlusOneLabel ->
            "+1"

        MinusOneLabel ->
            "-1"


type ButtonType
    = PlusButton
    | MinusButton


type MsgBlock
    = IncMsgBlock
    | DecMsgBlock


type ViewBlock modelType
    = ButtonBlock LabelType (Maybe MsgBlock)
    | LabelBlock (Maybe (ModelBlock modelType))


type FunctionBlock modelType
    = FunctionBlock (ModelBlock modelType -> ModelBlock modelType)


type UpdateBlock modelType
    = UpdateBlock MsgBlock (FunctionBlock modelType)


initialModelBlock : ModelBlock Int
initialModelBlock =
    ModelBlock 0


initialViewBlockList : ModelBlock modelType -> List (ViewBlock modelType)
initialViewBlockList modelBlock =
    [ ButtonBlock PlusOneLabel Nothing
    , LabelBlock <| Just modelBlock
    , ButtonBlock MinusOneLabel Nothing
    ]


updateBlockList : List (UpdateBlock Int)
updateBlockList =
    [ UpdateBlock IncMsgBlock <|
        FunctionBlock (\(ModelBlock content) -> ModelBlock <| content + 1)
    , UpdateBlock DecMsgBlock <|
        FunctionBlock (\(ModelBlock content) -> ModelBlock <| content - 1)
    ]


type AnimationMode
    = NoAnimation
    | UpdateAnimation ButtonType MsgBlock
    | ModelAnimation String MsgBlock
    | ViewAnimation


type alias Model =
    { modelBlock : ModelBlock Int
    , viewBlockList : List (ViewBlock Int)
    , updateBlockList : List (UpdateBlock Int)
    , sendMsgMaybe : Maybe MsgBlock
    , animationMode : AnimationMode
    , doFunctionMaybe : Maybe (FunctionBlock Int)
    , nextModelContent : String
    , selectedConnectButtonTypeMaybe : Maybe ButtonType
    }


initialModel : Model
initialModel =
    { modelBlock = initialModelBlock
    , viewBlockList = initialViewBlockList initialModelBlock
    , updateBlockList = updateBlockList
    , sendMsgMaybe = Nothing
    , animationMode = NoAnimation
    , doFunctionMaybe = Nothing
    , nextModelContent = ""
    , selectedConnectButtonTypeMaybe = Nothing
    }


type Msg
    = NoOp
    | Click ButtonType MsgBlock
    | ConnectClick (Maybe ButtonType)
    | ConnectEndClick MsgBlock
    | ApplyModel MsgBlock
    | ApplyView
    | StopMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Click buttonType msgBlock ->
            ( { model | sendMsgMaybe = Just msgBlock, animationMode = UpdateAnimation buttonType msgBlock }
            , Task.perform (always <| ApplyModel msgBlock) <| Process.sleep animationTime
            )

        ConnectClick buttonTypeMaybe ->
            ( { model | selectedConnectButtonTypeMaybe = buttonTypeMaybe }
            , Cmd.none
            )

        ConnectEndClick msgBlock ->
            case model.selectedConnectButtonTypeMaybe of
                Just selectedConnectButtonType ->
                    ( { model
                        | viewBlockList =
                            List.map
                                (\viewBlock ->
                                    case viewBlock of
                                        ButtonBlock labelType _ ->
                                            if labelType == PlusOneLabel && selectedConnectButtonType == PlusButton then
                                                ButtonBlock labelType <| Just msgBlock

                                            else if labelType == MinusOneLabel && selectedConnectButtonType == MinusButton then
                                                ButtonBlock labelType <| Just msgBlock

                                            else
                                                viewBlock

                                        _ ->
                                            viewBlock
                                )
                                model.viewBlockList
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        ApplyModel msgBlock ->
            let
                doFunctionMaybe =
                    List.head <|
                        List.concatMap
                            (\(UpdateBlock targetMsgBlock functionBlock) ->
                                if targetMsgBlock == msgBlock then
                                    [ functionBlock ]

                                else
                                    []
                            )
                            model.updateBlockList

                nextModelContent =
                    case doFunctionMaybe of
                        Just (FunctionBlock f) ->
                            let
                                (ModelBlock num) =
                                    f model.modelBlock
                            in
                            String.fromInt num

                        Nothing ->
                            ""
            in
            ( { model
                | doFunctionMaybe = doFunctionMaybe
                , modelBlock =
                    case doFunctionMaybe of
                        Just (FunctionBlock f) ->
                            f model.modelBlock

                        Nothing ->
                            model.modelBlock
                , nextModelContent = nextModelContent
                , animationMode =
                    case doFunctionMaybe of
                        Just (FunctionBlock f) ->
                            ModelAnimation nextModelContent msgBlock

                        Nothing ->
                            NoAnimation
              }
            , Task.perform (always ApplyView) <| Process.sleep animationTime
            )

        ApplyView ->
            ( { model
                | animationMode = ViewAnimation
                , viewBlockList =
                    case model.doFunctionMaybe of
                        Just (FunctionBlock f) ->
                            List.map
                                (\viewBlock ->
                                    case viewBlock of
                                        LabelBlock modelBlockMaybe ->
                                            LabelBlock <| Maybe.map f modelBlockMaybe

                                        _ ->
                                            viewBlock
                                )
                                model.viewBlockList

                        Nothing ->
                            model.viewBlockList
              }
            , Task.perform (always StopMsg) <| Process.sleep animationTime
            )

        StopMsg ->
            ( { model | sendMsgMaybe = Nothing, doFunctionMaybe = Nothing, animationMode = NoAnimation }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    svg [ width "1020", height "650", viewBox "50 -50 1020 750", onClick <| ConnectClick Nothing ] <|
        [ -- update
          g [ transform <| translate 100 150, Svg.Attributes.cursor "pointer" ]
            [ rect [ rx "50", ry "50", width "200", height "200", fill "#d99bff" ] []
            , line [ x1 "100", y1 "0", x2 "200", y2 "100", stroke "#cecccc", strokeWidth "3" ] []
            , line [ x1 "0", y1 "85", x2 "100", y2 "200", stroke "#cecccc", strokeWidth "3" ] []
            , text_ [ transform <| translate 130 -10 ] [ text "Inc" ]
            , text_ [ transform <| translate -30 125 ] [ text "Dec" ]
            , text_ [ transform <| translate 210 80 ] [ text "\\model -> model + 1" ]
            , text_ [ transform <| translate 110 220 ] [ text "\\model -> model - 1" ]
            ]
        , modelView model.modelBlock
        , viewView model.viewBlockList

        -- path
        , g []
            [ -- (Inc Lambda) -> model
              Svg.path [ id "inc_lambda_to_model_path", d "M 305 250 S 340 250, 340 270 V 515 S 340 545, 370 545", stroke "#cecccc", strokeWidth "3", fill "transparent" ] []

            -- (Dec Lambda) -> model
            , Svg.path [ id "dec_lambda_to_model_path", d "M 200 355 V 525 S 200 545, 240 545 H 390", stroke "#cecccc", strokeWidth "3", fill "transparent" ] []

            -- model -> view
            , Svg.path [ id "model_to_view_path", d "M 605 540 H 700 S 710 540, 710 520 V 355", stroke "#cecccc", strokeWidth "3", fill "transparent" ] []
            ]
        , case model.animationMode of
            UpdateAnimation buttonType msgBlock ->
                animateText
                    { id =
                        case ( buttonType, msgBlock ) of
                            ( PlusButton, IncMsgBlock ) ->
                                "#plus_button_to_inc_path"

                            ( PlusButton, DecMsgBlock ) ->
                                "#plus_button_to_dec_path"

                            ( MinusButton, IncMsgBlock ) ->
                                "#minus_button_to_inc_path"

                            ( MinusButton, DecMsgBlock ) ->
                                "#minus_button_to_dec_path"
                    , content =
                        case msgBlock of
                            IncMsgBlock ->
                                "Inc"

                            DecMsgBlock ->
                                "Dec"
                    }

            ModelAnimation content msgBlock ->
                animateText
                    { id =
                        case msgBlock of
                            IncMsgBlock ->
                                "#inc_lambda_to_model_path"

                            DecMsgBlock ->
                                "#dec_lambda_to_model_path"
                    , content = content
                    }

            ViewAnimation ->
                animateText { id = "#model_to_view_path", content = model.nextModelContent }

            NoAnimation ->
                text ""
        ]
            ++ pathViewList model.viewBlockList
            ++ clickButtonViewList model.animationMode model.viewBlockList
            ++ [ -- view polygon
                 polygon ([ onClickWithStopPropagation <| ConnectClick <| Just PlusButton, transform <| translate 660 105, points "20 45, 40 15, 60 45", fill "#84b7ff", Svg.Attributes.cursor "pointer" ] ++ selectedStroke PlusButton model.selectedConnectButtonTypeMaybe) []
               , polygon ([ onClickWithStopPropagation <| ConnectClick <| Just MinusButton, transform <| translate 745 200 ++ ", rotate(90, 50, 50)", points "20 45, 40 15, 60 45", fill "#84b7ff", Svg.Attributes.cursor "pointer" ] ++ selectedStroke MinusButton model.selectedConnectButtonTypeMaybe) []

               -- update polygon
               , polygon [ onClick <| ConnectEndClick IncMsgBlock, transform <| translate 140 66 ++ ", rotate(180, 50, 50)", points "20 45, 40 15, 60 45", fill "#ffd234", Svg.Attributes.cursor "pointer" ] []
               , polygon [ onClick <| ConnectEndClick DecMsgBlock, transform <| translate 15 195 ++ ", rotate(90, 50, 50)", points "20 45, 40 15, 60 45", fill "#ffd234", Svg.Attributes.cursor "pointer" ] []
               ]


selectedStroke : ButtonType -> Maybe ButtonType -> List (Attribute msg)
selectedStroke buttonType selectedConnectButtonTypeMaybe =
    if Just buttonType == selectedConnectButtonTypeMaybe then
        [ stroke "#ff9a62", strokeWidth "3" ]

    else
        []


onClickWithStopPropagation : msg -> Attribute msg
onClickWithStopPropagation msg =
    stopPropagationOn "click" <| JD.succeed ( msg, True )


pathViewList : List (ViewBlock Int) -> List (Html Msg)
pathViewList viewBlockList =
    List.filterMap
        (\viewBlock ->
            case viewBlock of
                ButtonBlock PlusOneLabel (Just IncMsgBlock) ->
                    Just <| Svg.path [ id "plus_button_to_inc_path", d "M 700 115 V 90 S 700 60, 680 60 H 220 S 200 60, 200 80 V 115", stroke "#cecccc", strokeWidth "3", fill "transparent" ] []

                ButtonBlock PlusOneLabel (Just DecMsgBlock) ->
                    Just <|
                        Svg.path
                            [ id "plus_button_to_dec_path", d "M 700 115 V 90 S 700 60, 680 60 H 20  S 0 60, 0 80 V 215 S 0 235, 20 235 H 60", stroke "#cecccc", strokeWidth "3", fill "transparent" ]
                            []

                ButtonBlock MinusOneLabel (Just IncMsgBlock) ->
                    Just <| Svg.path [ id "minus_button_to_inc_path", d "M 840 240 H 860 S 880 240, 880 210 V 30 S 880 10, 850 10 H 220 S 200 20, 200 40 V 115", stroke "#cecccc", strokeWidth "3", fill "transparent" ] []

                ButtonBlock MinusOneLabel (Just DecMsgBlock) ->
                    Just <| Svg.path [ id "minus_button_to_dec_path", d "M 835 240 H 860 S 880 240, 880 210 V 30 S 880 10, 850 10 H 20 S 0 20, 0 40 V 215 S 0 235, 20 235 H 60", stroke "#cecccc", strokeWidth "3", fill "transparent" ] []

                ButtonBlock _ Nothing ->
                    Nothing

                _ ->
                    Nothing
        )
        viewBlockList


clickButtonViewList : AnimationMode -> List (ViewBlock Int) -> List (Html Msg)
clickButtonViewList animationMode viewBlockList =
    List.filterMap
        (\viewBlock ->
            case viewBlock of
                ButtonBlock labelType msgBlockMaybe ->
                    let
                        clickEvent buttonType =
                            onClick <|
                                case ( msgBlockMaybe, animationMode ) of
                                    ( Just msgBlock, NoAnimation ) ->
                                        Click buttonType msgBlock

                                    _ ->
                                        NoOp
                    in
                    Just <|
                        case labelType of
                            PlusOneLabel ->
                                g [ clickEvent PlusButton, transform <| translate 680 160, Svg.Attributes.cursor "pointer" ]
                                    [ rect [ rx "5", ry "5", width "40", height "30", fill "#efefef", stroke "#767676" ] []
                                    , text_ [ transform <| translate 10 20 ] [ text "+1" ]
                                    ]

                            MinusOneLabel ->
                                g [ clickEvent MinusButton, transform <| translate 750 225, Svg.Attributes.cursor "pointer" ]
                                    [ rect [ rx "5", ry "5", width "40", height "30", fill "#efefef", stroke "#767676" ] []
                                    , text_ [ transform <| translate 10 20 ] [ text "-1" ]
                                    ]

                _ ->
                    Nothing
        )
        viewBlockList


viewView : List (ViewBlock Int) -> Html msg
viewView viewBlockList =
    g [ transform <| translate 600 150 ] <|
        List.concat <|
            List.filterMap
                (\viewBlock ->
                    case viewBlock of
                        LabelBlock (Just (ModelBlock modelContent)) ->
                            Just <|
                                [ rect [ rx "50", ry "50", width "200", height "200", fill "#4ecb71" ] []
                                , text_ [ transform <| translate 90 100, fontSize "30" ] [ tspan [ fontWeight "bold" ] [ text <| String.fromInt modelContent ] ]
                                ]

                        _ ->
                            Nothing
                )
                viewBlockList


modelView : ModelBlock Int -> Html msg
modelView (ModelBlock modelContent) =
    g [ transform <| translate 400 450 ]
        [ rect [ rx "50", ry "50", width "200", height "200", fill "#ff9a62" ] []
        , text_ [ transform <| translate 20 100, fontSize "30" ] [ tspan [ fontWeight "bold" ] [ text <| "model = " ++ String.fromInt modelContent ] ]
        ]


animateText : { id : String, content : String } -> Html msg
animateText { id, content } =
    text_ [ fill "#d83d0f", fontSize "20px" ]
        [ tspan [ fontWeight "bold" ]
            [ text content
            ]
        , animateMotion [ dur "2.5s", repeatCount "indefinite" ]
            [ mpath [ xlinkHref id ] []
            ]
        ]


translate : Float -> Float -> String
translate x y =
    "translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")"


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
