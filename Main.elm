module Main exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)


-- TREES


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


empty : Tree a
empty =
    Empty


singleton : a -> Tree a
singleton v =
    Node v Empty Empty


insert : comparable -> Tree comparable -> Tree comparable
insert x tree =
    case tree of
        Empty ->
            singleton x

        Node y left right ->
            if x > y then
                Node y left (insert x right)
            else if x < y then
                Node y (insert x left) right
            else
                tree


fromList : List comparable -> Tree comparable
fromList xs =
    List.foldl insert empty xs


depth : Tree a -> Int
depth tree =
    case tree of
        Empty ->
            0

        Node v left right ->
            1 + max (depth left) (depth right)


map : (a -> b) -> Tree a -> Tree b
map f tree =
    case tree of
        Empty ->
            Empty

        Node v left right ->
            Node (f v) (map f left) (map f right)



-- PLAYGROUND


deepTree : Tree Int
deepTree =
    fromList [ 1, 2, 3 ]


niceTree : Tree Int
niceTree =
    fromList [ 2, 1, 3 ]


main : Html msg
main =
    div [ style [ ( "font-family", "monospace" ) ] ]
        [ display "depth deepTree" (depth deepTree)
        , display "depth niceTree" (depth niceTree)
        , display "incremented" (map (\n -> n + 1) niceTree)
        ]


display : String -> a -> Html msg
display name value =
    div [] [ text (name ++ " ==> " ++ toString value) ]


mytesttree : Tree Int
mytesttree =
    singleton 5
        |> insert 3
        |> insert 4
        |> insert 7
        |> insert 9
        |> insert 6



{- Exercises:

   (1) Sum all of the elements of a tree.

          sum : Tree number -> number-
-}


sum : Tree number -> number
sum tree =
    case tree of
        Empty ->
            0

        Node x left right ->
            x + sum left + sum right



{- (2) Flatten a tree into a list.

   flatten : Tree a -> List a
-}


flatten : Tree a -> List a
flatten tree =
    case tree of
        Empty ->
            []

        Node x left right ->
            List.append [ x ] (flatten right)
                |> List.append (flatten left)



{- (3) Check to see if an element is in a given tree.

   isElement : a -> Tree a -> Bool
-}


contains : Tree a -> a -> Bool
contains tree item =
    case tree of
        Empty ->
            False

        Node x left right ->
            if x == item then
                True
            else
                (contains left item) || (contains right item)



{- (4) Write a general fold function that acts on trees. The fold
   function does not need to guarantee a particular order of
   traversal.

      fold : (a -> b -> b) -> b -> Tree a -> b
-}


fold : (a -> b -> b) -> b -> Tree a -> b
fold function accumulator tree =
    case tree of
        Empty ->
            accumulator

        Node x left right ->
            let
                thisNode =
                    function x accumulator
            in
                fold function (fold function thisNode left) right



{- (5) Use "fold" to do exercises 1-3 in one line each. The best
   readable versions I have come up have the following length
   in characters including spaces and function name:
     sum: 16
     flatten: 21
     isElement: 46
   See if you can match or beat me! Don't forget about currying
   and partial application!
-}
--I'm not big on code golf but these example show how it can be cleaner to implement
--some of these functions with fold.
--Note that I'm not using any partial application here, as I didn't think it added clarity
--and was mostly suggested for the purposes of code golf


foldSum : Tree Int -> Int
foldSum tree =
    fold (+) 0 tree


foldFlatten : Tree a -> List a
foldFlatten tree =
    fold (::) [] tree


foldContains : Tree a -> a -> Bool
foldContains tree item =
    fold ((==) item >> (||)) False tree



--(6) Can "fold" be used to implement "map" or "depth"?
--TODO
--(7) Try experimenting with different ways to traverse a
--    tree: pre-order, in-order, post-order, depth-first, etc.
--    More info at: http://en.wikipedia.org/wiki/Tree_traversal


preOrder : Tree a -> List a
preOrder tree =
    case tree of
        Empty ->
            []

        Node x left right ->
            List.append (x :: (preOrder left)) (preOrder right)


inOrder : Tree a -> List a
inOrder tree =
    case tree of
        Empty ->
            []

        Node x left right ->
            List.append (List.append (inOrder left) [ x ]) (inOrder right)


postOrder : Tree a -> List a
postOrder tree =
    case tree of
        Empty ->
            []

        Node x left right ->
            List.append (List.append (postOrder left) (postOrder right)) [ x ]



--TODO: breadth first search
