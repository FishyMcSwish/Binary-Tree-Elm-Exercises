module TreeTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Main exposing (..)


suite : Test
suite =
    describe "testing stuff"
        [ describe "tests for sum, filter, contains"
            [ test "sum up stuff" <|
                \_ ->
                    let
                        tree =
                            mytesttree
                    in
                        Expect.equal 34 (sum tree)
            ]
        , describe "tests for flatten"
            [ test "flatten to a list" <|
                \_ ->
                    Expect.equal [ 3, 4, 5, 6, 7, 9 ] (flatten mytesttree)
            ]
        , describe "some tests for contains"
            [ test "when the tree is empty it should return false" <|
                \_ ->
                    Expect.equal False (contains empty 1)
            , test "find the thing in a singleton" <|
                \_ -> Expect.equal True (contains (singleton 1) 1)
            , test "find the thing in a bigger tree" <|
                \_ -> Expect.equal True (contains mytesttree 3)
            , test "don't find things that aren't there" <|
                \_ -> Expect.equal False (contains mytesttree 10)
            ]
        , describe "tests for fold"
            [ test "just return starting value when tree is empty" <|
                \_ -> Expect.equal 0 (fold (\x y -> x + y) 0 empty)
            , test "test folding: adding a singleton" <|
                \_ -> Expect.equal 4 (fold (\x y -> x + y) 0 (singleton 4))
            , test "test folding: adding a larger tree" <|
                \_ -> Expect.equal 34 (fold (\x y -> x + y) 0 mytesttree)
            ]
        , describe "tests for foldsum"
            [ test "sums stuff" <|
                \_ -> Expect.equal 34 (foldSum mytesttree)
            ]
        , describe "tests for fold-flatten"
            [ test "flatten to a list" <|
                \_ ->
                    Expect.equal [ 9, 6, 7, 4, 3, 5 ] (foldFlatten mytesttree)
            ]
        , describe "some tests for fold contains"
            [ test "when the tree is empty it should return false" <|
                \_ ->
                    Expect.equal False (foldContains empty 1)
            , test "find the thing in a singleton" <|
                \_ -> Expect.equal True (foldContains (singleton 1) 1)
            , test "find the thing in a bigger tree" <|
                \_ -> Expect.equal True (foldContains mytesttree 3)
            , test "don't find things that aren't there" <|
                \_ -> Expect.equal False (foldContains mytesttree 10)
            ]
        , describe "tests for different traversal methods"
            [ test "pre-order" <|
                \_ ->
                    Expect.equal [ 5, 3, 4, 7, 6, 9 ] (preOrder mytesttree)
            , test "in-order" <|
                \_ ->
                    Expect.equal [ 3, 4, 5, 6, 7, 9 ] (inOrder mytesttree)
            , test "post-order" <|
                \_ ->
                    Expect.equal [ 4, 3, 6, 9, 7, 5 ] (postOrder mytesttree)
            ]
        , describe "tests for map"
            [ test "map adding 1" <|
                \_ ->
                    let
                        newTree =
                            preOrder
                                (map (\n -> n + 1) mytesttree)
                    in
                        Expect.equal [ 6, 4, 5, 8, 7, 10 ] newTree
            , test "foldMap adding 1" <|
                \_ ->
                    let
                        newTree =
                            preOrder
                                (foldMap (\n -> n + 1) mytesttree)
                    in
                        Expect.equal [ 6, 4, 5, 8, 7, 10 ] newTree
            ]
        , describe "tests for foldDepth"
            [ test "depth of mytestttee" <|
                \_ ->
                    Expect.equal (depth mytesttree) (foldDepth mytesttree)
            , test "depth of deepTree" <|
                \_ ->
                    Expect.equal (depth deepTree) (foldDepth deepTree)
            , test "depth of niceTree" <|
                \_ ->
                    Expect.equal (depth niceTree) (foldDepth niceTree)
            ]
        ]
