module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events



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


init : Model
init =
    { tree = Empty, input = "", log = [] }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
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


insertValue : Int -> Tree Int -> ( Tree Int, String )
insertValue value tree =
    case tree of
        Empty ->
            ( Node Empty value Empty
            , String.fromInt value ++ " was inserted into the tree"
            )

        Node left nodeVal right ->
            case compVal value nodeVal of
                Less ->
                    let
                        ( l, str ) =
                            insertValue value left
                    in
                    ( Node l nodeVal right, str )

                Equal ->
                    ( Node left nodeVal right
                    , String.fromInt nodeVal ++ " was already in the tree"
                    )

                Greater ->
                    let
                        ( r, str ) =
                            insertValue value right
                    in
                    ( Node left nodeVal r, str )


insert : Int -> Model -> Model
insert value model =
    let
        ( newTree, newLog ) =
            insertValue value model.tree
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


deleteValue : Int -> Tree Int -> ( Tree Int, String )
deleteValue value tree =
    case tree of
        Empty ->
            ( Empty, String.fromInt value ++ " was not found" )

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
                            String.fromInt value ++ " was found and deleted"
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


delete : Int -> Model -> Model
delete val model =
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
                depthLeft =
                    depth left

                depthRight =
                    depth right
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
                Nothing ->
                    { model
                        | input = ""
                        , log = (model.input ++ " is not a valid integer") :: model.log
                    }

                Just value ->
                    insert value model

        Delete ->
            case String.toInt model.input of
                Nothing ->
                    { model
                        | input = ""
                        , log = (model.input ++ " is not a valid integer") :: model.log
                    }

                Just value ->
                    delete value model

        AddAll ->
            case String.toInt model.input of
                Nothing ->
                    { model
                        | input = ""
                        , log = (model.input ++ " is not a valid integer") :: model.log
                    }

                Just value ->
                    { model
                        | input = ""
                        , tree = mapTree (\n -> n + value) model.tree
                        , log = (String.fromInt value ++ " added to values in the tree") :: model.log
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


mapAttributeStyles : List ( String, String ) -> List (Attribute Msg)
mapAttributeStyles =
    List.map (\( a, b ) -> Attributes.style a b)


buttonStyle : List (Attribute Msg)
buttonStyle =
    mapAttributeStyles [ ( "height", "30px" ), ( "width", "100%" ) ]


inputStyle : List (Attribute Msg)
inputStyle =
    mapAttributeStyles
        [ ( "height", "30px" )
        , ( "width", "97%" )
        ]


titleStyle : List (Attribute Msg)
titleStyle =
    mapAttributeStyles
        [ ( "height", "10%" )
        , ( "text", "bold" )
        ]


nodeStyle : List (Attribute Msg)
nodeStyle =
    mapAttributeStyles
        [ ( "border-color", "black" )
        , ( "border-width", "1px" )
        , ( "border-style", "solid" )
        ]



-- End Styles


buildButton : Msg -> String -> Html Msg
buildButton msg txt =
    div (Attributes.class "pure-button-active" :: buttonStyle)
        [ button (Events.onClick msg :: buttonStyle) [ text txt ] ]


inputsView : Model -> Html Msg
inputsView model =
    div [ Attributes.class "pure-u-1-5" ]
        [ input
            ([ Attributes.type_ "text"
             , Attributes.placeholder "Value must be an integer"
             , Attributes.value model.input
             , Events.onInput Uncommitted
             ]
                ++ inputStyle
            )
            []
        , buildButton Insert "Insert value into tree"
        , buildButton Delete "Delete value from tree"
        , buildButton AddAll "Add value to all values"
        , buildButton Clear "Clear tree of all values"
        ]


displayTree : Tree Int -> List (Html Msg)
displayTree tree =
    case tree of
        Empty ->
            [ text "-" ]

        Node left value right ->
            [ div (Attributes.class "pure-u-1-1" :: nodeStyle)
                [ text (String.fromInt value) ]
            , div [ Attributes.class "pure-u-1-2" ]
                (displayTree left)
            , div [ Attributes.class "pure-u-1-2" ]
                (displayTree right)
            ]


displayView : Model -> Html Msg
displayView model =
    div [ Attributes.class "pure-u-3-5", Attributes.align "center" ]
        [ div titleStyle [ text "Binary Tree" ]
        , div [ Attributes.align "center" ] (displayTree model.tree)
        ]


logView : Model -> Html Msg
logView model =
    let
        divLog =
            div [ Attributes.class "pure-u-1-5", Attributes.align "right" ]
    in
    let
        buildText log =
            case log of
                [] ->
                    [ div [ Attributes.align "right" ] [ text "" ] ]

                hd :: tl ->
                    div [ Attributes.align "right" ] [ text (hd ++ "\n") ] :: buildText tl
    in
    divLog (buildText model.log)


headerView : Html Msg
headerView =
    div [ Attributes.class "Display" ] [ text "Binary Tree" ]


view : Model -> Html Msg
view model =
    div [ Attributes.class "pure-g" ]
        [ inputsView model
        , displayView model
        , logView model
        ]
