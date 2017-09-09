module Example exposing (..)

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


thenEnsure : String -> (a -> Expectation) -> Maybe a -> Expectation
thenEnsure onFail expecting x =
    case x of
        Nothing ->
            Expect.fail onFail

        Just y ->
            expecting y


suite : Test
suite =
    describe "Module Tree tests"
        [ describe "Tree.goToLeftChild"
            [ test "applied once should go to the node 11" <|
                \_ ->
                    zip tree
                        |> goToLeftChild
                        |> thenEnsure "Couldn't reach child"
                            (\child ->
                                Tree.unzip child |> firstElement |> Expect.equal 11
                            )
            , test "applied twice should go to the node 111" <|
                \_ ->
                    zip tree
                        |> goToLeftChild
                        |> andThen goToLeftChild
                        |> thenEnsure "Couldn't reach child"
                            (\child ->
                                Tree.unzip child |> firstElement |> Expect.equal 111
                            )
            , test "applied three times should find Nothing" <|
                \_ ->
                    zip tree
                        |> goToLeftChild
                        |> andThen goToLeftChild
                        |> andThen goToLeftChild
                        |> Expect.equal Nothing
            ]
        , describe "Tree.goToRightChild"
            [ test "applied once should go to the node 12" <|
                \_ ->
                    zip tree
                        |> goToRightChild
                        |> thenEnsure "Couldn't reach child"
                            (\child ->
                                Tree.unzip child |> firstElement |> Expect.equal 12
                            )
            , test "applied twice should go to the node 122" <|
                \_ ->
                    zip tree
                        |> goToRightChild
                        |> andThen goToRightChild
                        |> thenEnsure "Couldn't reach child"
                            (\child ->
                                Tree.unzip child |> firstElement |> Expect.equal 122
                            )
            , test "applied three times should find Nothing" <|
                \_ ->
                    zip tree
                        |> goToRightChild
                        |> andThen goToRightChild
                        |> andThen goToRightChild
                        |> Expect.equal Nothing
            ]
        , let
            down =
                zip tree
                    |> goToRightChild
                    |> andThen goToLeftChild
                    |> andThen goToRightChild
          in
          describe "Tree.goUp"
            [ test "applied once should go to the node 121" <|
                \_ ->
                    down
                        |> andThen goUp
                        |> thenEnsure "Couldn't reach upper level"
                            (\upper -> Tree.unzip upper |> firstElement |> Expect.equal 121)
            ]

        -- [ fuzzGraph "Fuzz navigation" (zip <| Leaf 1) <|
        --     empty
        --         |> insertNodeData 11
        --             (Modify <|
        --                 unzip
        --                     |> flip (<:) ( Leaf 111, Leaf 112 )
        --                     |> zip
        --             )
        --         |> insertNodeData 12 (Modify <| goToLeftChild)
        --         |> insertNodeData 13
        --             (Expect <|
        --                 \zipper ->
        --                     zipper
        --                         |> unzip
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
        --                 unzip
        --                     |> flip (<:) ( Leaf 211, Leaf 212 )
        --                     |> zip
        --             )
        --         |> insertNodeData 22 (Modify <| goToRightChild)
        --         |> insertNodeData 23
        --             (Expect <|
        --                 \zipper ->
        --                     zipper
        --                         |> unzip
        --                         |> firstElement
        --                         |> Expect.equal 212
        --             )
        --         |> insertNodeData 24 (Modify <| goToLeftChild)
        --         |> insertEdge ( 21, 22 )
        --         |> insertEdge ( 22, 23 )
        --         |> insertEdge ( 23, 24 )
        --         --
        --         |> insertNodeData 31 (Modify <| goToRoot)
        --         |> insertNodeData 32 (Expect <| unzip |> Expect.equal 1)
        --         |> insertEdge ( 31, 32 )
        ]
