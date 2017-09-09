module TestTree exposing (..)

--import Fuzz exposing (Fuzzer, int, list, string)
--import Graph exposing (empty, insertEdge, insertNodeData)
--import List exposing (unzip)
--import Test.Graph exposing (Action(Modify), fuzzGraph)

import Expect exposing (Expectation)
import Maybe exposing (andThen)
import Test exposing (..)
import Tree exposing (..)


(<:) : a -> ( Tree a, Tree a ) -> Tree a
(<:) root ( left, right ) =
    Node root left right


tree : Tree number
tree =
    1
        <: ( 11
                <: ( Leaf 111, Leaf 112 )
           , 12
                <: ( 121
                        <: ( Leaf 1211, Leaf 1212 )
                   , Leaf 122
                   )
           )


zipper =
    zip tree


thenEnsure : (a -> Expectation) -> String -> Maybe a -> Expectation
thenEnsure expecting onFail m =
    case m of
        Nothing ->
            Expect.fail onFail

        Just y ->
            expecting y


thenCompare : a -> String -> Maybe (Zipper a) -> Expectation
thenCompare value onFail =
    thenEnsure (Expect.equal value << element) onFail


suite : Test
suite =
    describe "Module Tree tests"
        [ describe "Tree.goToLeftChild"
            [ test "applied once should go to the node 11" <|
                \_ ->
                    zipper
                        |> goToLeftChild
                        |> thenCompare 11 "Couldn't reach child"
            , test "applied twice should go to the node 111" <|
                \_ ->
                    zipper
                        |> goToLeftChild
                        |> andThen goToLeftChild
                        |> thenCompare 111 "Couldn't reach child"
            , test "applied three times should find Nothing" <|
                \_ ->
                    zipper
                        |> goToLeftChild
                        |> andThen goToLeftChild
                        |> andThen goToLeftChild
                        |> Expect.equal Nothing
            ]
        , describe "Tree.goToRightChild"
            [ test "applied once should go to the node 12" <|
                \_ ->
                    zipper
                        |> goToRightChild
                        |> thenCompare 12 "Couldn't reach child"
            , test "applied twice should go to the node 122" <|
                \_ ->
                    zipper
                        |> goToRightChild
                        |> andThen goToRightChild
                        |> thenCompare 122 "Couldn't reach child"
            , test "applied three times should find Nothing" <|
                \_ ->
                    zipper
                        |> goToRightChild
                        |> andThen goToRightChild
                        |> andThen goToRightChild
                        |> Expect.equal Nothing
            ]
        , let
            down =
                zipper
                    |> goToRightChild
                    |> andThen goToLeftChild
                    |> andThen goToRightChild
          in
          describe "Tree.goUp"
            [ test "applied once should go to the node 121" <|
                \_ ->
                    down
                        |> andThen goUp
                        |> thenCompare 121 "Couldn't reach upper level"
            , test "applied twice should go to the node 12" <|
                \_ ->
                    down
                        |> andThen goUp
                        |> andThen goUp
                        |> thenCompare 12 "Couldn't reach upper level"
            , test "applied three times should go to the node 1" <|
                \_ ->
                    down
                        |> andThen goUp
                        |> andThen goUp
                        |> andThen goUp
                        |> thenCompare 1 "Couldn't reach upper level"
            , test "applied four times should find Nothing" <|
                \_ ->
                    down
                        |> andThen goUp
                        |> andThen goUp
                        |> andThen goUp
                        |> andThen goUp
                        |> Expect.equal Nothing
            ]
        , let
            thenTest =
                thenEnsure
                    (goToRoot >> element >> Expect.equal 1)
                    "Couldn't reach root"
          in
          describe "Tree.goToRoot"
            [ test "should go up one level properly" <|
                \_ -> zipper |> goToRightChild |> thenTest
            , test "should go up two levels properly" <|
                \_ ->
                    zipper
                        |> goToRightChild
                        |> andThen goToLeftChild
                        |> thenTest
            , test "should go up three levels properly" <|
                \_ ->
                    zipper
                        |> goToRightChild
                        |> andThen goToLeftChild
                        |> andThen goToLeftChild
                        |> thenTest
            , test "should stay at root" <|
                \_ ->
                    zipper
                        |> goToRoot
                        |> element
                        |> Expect.equal 1
            ]
        , describe "Tree.goTo"
            [ test "should find the node 1211" <|
                \_ ->
                    goTo ((==) 1211) zipper
                        |> thenCompare 1211 "Couldn't find node 1211"
            , test "should find the node 112" <|
                \_ ->
                    goTo (\n -> rem n 2 == 0) zipper
                        |> thenCompare 112 "Couldn't find node 112"
            , test "should find Nothing" <|
                \_ ->
                    goTo ((==) 17) zipper
                        |> Expect.equal Nothing
            ]

        -- [ fuzzGraph "Fuzz navigation" (zip <| Leaf 1) <|
        --     empty
        --         |> insertNodeData 11
        --             (Modify <|
        --                 subtree
        --                     |> flip (<:) ( Leaf 111, Leaf 112 )
        --                     |> zip
        --             )
        --         |> insertNodeData 12 (Modify <| goToLeftChild)
        --         |> insertNodeData 13
        --             (Expect <|
        --                 \zipper ->
        --                     zipper
        --                         |> subtree
        --                         |> firstElement
        --                         |> Expect.equal 111
        --             )
        --         |> insertNodeData 14 (Modify <| goToRightChild)
        --         |> insertEdge ( 11, 12 )
        --         |> insertEdge ( 12, 13 )
        --         |> insertEdge ( 13, 14 )
        --         --
        --         |> insertNodeData 21
        --             (Modify <|
        --                 subtree
        --                     |> flip (<:) ( Leaf 211, Leaf 212 )
        --                     |> zip
        --             )
        --         |> insertNodeData 22 (Modify <| goToRightChild)
        --         |> insertNodeData 23
        --             (Expect <|
        --                 \zipper ->
        --                     zipper
        --                         |> subtree
        --                         |> firstElement
        --                         |> Expect.equal 212
        --             )
        --         |> insertNodeData 24 (Modify <| goToLeftChild)
        --         |> insertEdge ( 21, 22 )
        --         |> insertEdge ( 22, 23 )
        --         |> insertEdge ( 23, 24 )
        --         --
        --         |> insertNodeData 31 (Modify <| goToRoot)
        --         |> insertNodeData 32 (Expect <| subtree |> Expect.equal 1)
        --         |> insertEdge ( 31, 32 )
        ]
