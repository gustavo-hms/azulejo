module Tree exposing (..)

import Maybe exposing (andThen)


type Tree a
    = Leaf a
    | Node a (Tree a) (Tree a)


type Crumb a
    = LeftCrumb a (Tree a)
    | RightCrumb a (Tree a)


type alias Breadcrumbs a =
    List (Crumb a)


type alias Zipper a =
    ( Tree a, Breadcrumbs a )


goToLeftChild : Zipper a -> Maybe (Zipper a)
goToLeftChild ( tree, breadcrumbs ) =
    case tree of
        Leaf element ->
            Nothing

        Node element leftTree rightTree ->
            Just ( leftTree, LeftCrumb element rightTree :: breadcrumbs )


goToRightChild : Zipper a -> Maybe (Zipper a)
goToRightChild ( tree, breadcrumbs ) =
    case tree of
        Leaf element ->
            Nothing

        Node element leftTree rightTree ->
            Just ( rightTree, RightCrumb element leftTree :: breadcrumbs )


goUp : Zipper a -> Maybe (Zipper a)
goUp ( tree, breadcrumbs ) =
    case breadcrumbs of
        [] ->
            Nothing

        (LeftCrumb element rightTree) :: breadcrumbs ->
            Just ( Node element tree rightTree, breadcrumbs )

        (RightCrumb element leftTree) :: breadcrumbs ->
            Just ( Node element tree leftTree, breadcrumbs )


goToRoot : Zipper a -> Maybe (Zipper a)
goToRoot (( _, breadcrumbs ) as zipper) =
    case breadcrumbs of
        [] ->
            Just zipper

        breadcrumb :: breadcrumbs ->
            goUp zipper |> andThen goToRoot


goTo : (a -> Bool) -> Zipper a -> Maybe (Zipper a)
goTo predicate zipper =
    let
        root =
            goToRoot zipper

        rootElement tree =
            case tree of
                Leaf a ->
                    a

                Node a _ _ ->
                    a

        orElse takeThis tryThat =
            case tryThat of
                Just _ ->
                    tryThat

                Nothing ->
                    takeThis

        find predicate (( tree, _ ) as zipper) =
            if predicate <| rootElement tree then
                Just zipper
            else
                goToLeftChild zipper
                    |> andThen (find predicate)
                    |> orElse (goToRightChild zipper |> andThen (find predicate))
    in
    root |> andThen (find predicate)
