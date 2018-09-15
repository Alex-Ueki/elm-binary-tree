module Main exposing (Comp(..), Model, Msg(..), Tree(..), buildButton, buttonStyle, compVal, delete, depth, displayTree, displayView, findValue, headerView, inputStyle, inputsView, insert, logView, main, mapTree, max, min, model, nodeStyle, titleStyle, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import String



-- BASE TYPE DEFINITIONS

-- Defines the comparison of values into 3 types
type Comp
    = Less
    | Equal
    | Greater


-- Binary trees have a recursive type definition
-- the 'a' means the type can be anything, but in practice this is an int
-- You can use this for other values as long as you implement a comparator method
-- i.e compare: a -> a -> Comp
type Tree a
    = Empty
    | Node (Tree a) a (Tree a)


-- Model contains an integer binary tree, current input, and a log
type alias Model =
    { tree : Tree Int, input : String, log : List String }


model : Model
model =
    { tree = Empty, input = "", log = [] }


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- init
-- TREE FUNCTIONS


compVal : Int -> Int -> Comp
compVal val nodeVal =
    if val < nodeVal then
        Less

    else if val == nodeVal then
        Equal

    else
        Greater


insert : Int -> Model -> Model
insert value model =
    let
        insertValue tree =
            case tree of
                Empty ->
                    ( Node Empty value Empty
                    , toString value ++ " was inserted into the tree"
                    )

                Node left nodeVal right ->
                    case compVal value nodeVal of
                        Less ->
                            let
                                ( l, str ) =
                                    insertValue left
                            in
                            ( Node l nodeVal right, str )

                        Equal ->
                            ( Node left nodeVal right
                            , toString nodeVal ++ " was already in the tree"
                            )

                        Greater ->
                            let
                                ( r, str ) =
                                    insertValue right
                            in
                            ( Node left nodeVal r, str )
    in
    let
        ( newTree, newLog ) =
            insertValue model.tree
    in
    { model
        | input = ""
        , tree = newTree
        , log = newLog :: model.log
    }



-- insertValue : Int -> Tree Int -> Tree Int


findValue : Int -> Tree Int -> Bool
findValue value tree =
    case tree of
        Empty ->
            False

        Node left nodeVal right ->
            case compVal value nodeVal of
                Less ->
                    findValue value left

                Equal ->
                    True

                Greater ->
                    findValue value right


min : Tree Int -> Maybe Int
min tree =
    case tree of
        Empty ->
            Nothing

        Node left nodeVal _ ->
            case left of
                Empty ->
                    Just nodeVal

                Node next _ _ ->
                    min next


max : Tree Int -> Maybe Int
max tree =
    case tree of
        Empty ->
            Nothing

        Node _ nodeVal right ->
            case right of
                Empty ->
                    Just nodeVal

                Node _ _ next ->
                    max next


delete : Int -> Model -> Model
delete val model =
    let
        deleteValue value tree =
            case tree of
                Empty ->
                    ( Empty, toString value ++ " was not found" )

                Node left nodeVal right ->
                    case compVal value nodeVal of
                        Less ->
                            let
                                ( l, str ) =
                                    deleteValue value left
                            in
                            ( Node l nodeVal right, str )

                        Greater ->
                            let
                                ( r, str ) =
                                    deleteValue value right
                            in
                            ( Node left nodeVal r, str )

                        Equal ->
                            let
                                str =
                                    toString value ++ " was found and deleted"
                            in
                            case max left of
                                -- Check left tree
                                Just maxLeft ->
                                    let
                                        ( replacement, _ ) =
                                            deleteValue maxLeft left
                                    in
                                    ( Node replacement maxLeft right, str )

                                Nothing ->
                                    case min right of
                                        Just minRight ->
                                            let
                                                ( replacement, _ ) =
                                                    deleteValue minRight right
                                            in
                                            ( Node left minRight replacement, str )

                                        Nothing ->
                                            ( Empty, str )
    in
    let
        ( newTree, newLog ) =
            deleteValue val model.tree
    in
    { model
        | input = ""
        , tree = newTree
        , log = newLog :: model.log
    }


depth : Tree a -> Int
depth tree =
    case tree of
        Empty ->
            0

        Node left _ right ->
            let
                depthLeft = depth left
                depthRight = depth right
            in
            if depthLeft < depthRight then
                1 + depthRight

            else
                1 + depthLeft


-- Maps a function to all values in the tree
mapTree : (a -> a) -> Tree a -> Tree a
mapTree func tree =
    case tree of
        Empty ->
            Empty

        Node left nodeVal right ->
            Node (mapTree func left) (func nodeVal) (mapTree func right)



-- UPDATE


type Msg
    = Uncommitted String
    | Insert
    | Delete
    | AddAll
    | Clear


update : Msg -> Model -> Model
update msg model =
    case msg of
        Uncommitted str ->
            { model | input = str }

        Insert ->
            case String.toInt model.input of
                Err e ->
                    { model
                        | input = ""
                        , log = e :: model.log
                    }

                Ok value ->
                    insert value model

        Delete ->
            case String.toInt model.input of
                Err e ->
                    { model
                        | input = ""
                        , log = e :: model.log
                    }

                Ok value ->
                    delete value model

        AddAll ->
            case String.toInt model.input of
                Err e ->
                    { model
                        | input = ""
                        , log = e :: model.log
                    }

                Ok value ->
                    { model
                        | input = ""
                        , tree = mapTree (\n -> n + value) model.tree
                        , log = (toString value ++ " added to values in the tree") :: model.log
                    }

        Clear ->
            { model
                | tree = Empty
                , input = ""
                , log = "Cleared all values from tree" :: model.log
            }



-- VIEW
-- div : List Attribute -> List Html -> Html
-- Styles


buttonStyle : Attribute Msg
buttonStyle =
    style
        [ ( "height", "30px" )
        , ( "width", "100%" )
        ]


inputStyle : Attribute Msg
inputStyle =
    style
        [ ( "height", "30px" )
        , ( "width", "97%" )
        ]


titleStyle : Attribute Msg
titleStyle =
    style
        [ ( "height", "10%" )
        , ( "text", "bold" )
        ]


nodeStyle : Attribute Msg
nodeStyle =
    style
        [ ( "border-color", "black" )
        , ( "border-width", "1px" )
        , ( "border-style", "solid" )
        ]



-- End Styles


buildButton : Msg -> String -> Html Msg
buildButton msg txt =
    div [ buttonStyle, class "pure-button-active" ]
        [ button [ onClick msg, buttonStyle ] [ text txt ] ]


inputsView : Model -> Html Msg
inputsView model =
    div [ class "pure-u-1-5" ]
        [ input
            [ type_ "text"
            , placeholder "Value must be an integer"
            , value model.input
            , onInput Uncommitted
            , inputStyle
            ]
            []
        , buildButton Insert "Insert value into tree"
        , buildButton Delete "Delete value from tree"
        , buildButton AddAll "Add value to all values"
        , buildButton Clear "Clear tree of all values"
        ]


displayTree : Tree a -> List (Html Msg)
displayTree tree =
    case tree of
        Empty ->
            [ text "-" ]

        Node left value right ->
            [ div [ class "pure-u-1-1", nodeStyle ]
                [ text (toString value) ]
            , div [ class "pure-u-1-2" ]
                (displayTree left)
            , div [ class "pure-u-1-2" ]
                (displayTree right)
            ]


displayView : Model -> Html Msg
displayView model =
    div [ class "pure-u-3-5", align "center" ]
        [ div [ titleStyle ] [ text "Binary Tree" ]
        , text (toString model.tree)
        , div [ align "center" ] (displayTree model.tree)
        ]


logView : Model -> Html Msg
logView model =
    let
        divLog =
            div [ class "pure-u-1-5", align "right" ]
    in
    let
        buildText log =
            case log of
                [] ->
                    -- [List Html]
                    [ div [ align "right" ] [ text "" ] ]

                hd :: tl ->
                    div [ align "right" ] [ text (hd ++ "\n") ] :: buildText tl
    in
    divLog (buildText model.log)


headerView : Html Msg
headerView =
    div [ class "Display" ] [ text "Binary Tree" ]


view : Model -> Html Msg
view model =
    div [ class "pure-g" ]
        [ inputsView model
        , displayView model
        , logView model
        ]
