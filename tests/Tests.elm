module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import AStar exposing (..)
import Dict
import Set
import String


genGrid : List comparable -> Dict.Dict comparable ()
genGrid coords =
    Dict.fromList (List.map (\x -> ( x, () )) coords)


grid1 : Dict.Dict ( number, number ) ()
grid1 =
    genGrid [ ( 1, 1 ), ( 2, 1 ) ]


grid2 : Dict.Dict ( number, number ) ()
grid2 =
    genGrid [ ( 1, 3 ), ( 2, 3 ), ( 3, 3 ), ( 1, 2 ), ( 1, 1 ), ( 2, 1 ), ( 3, 1 ) ]


grid3 : Dict.Dict ( number, number ) ()
grid3 =
    genGrid [ ( 1, 3 ), ( 3, 3 ), ( 1, 2 ), ( 2, 2 ), ( 3, 2 ), ( 1, 1 ), ( 1, 2 ), ( 1, 3 ) ]


getNeighbors2d : Dict.Dict ( number, number ) a -> ( number, number ) -> List ( ( number, number ), number )
getNeighbors2d dict ( x, y ) =
    let
        possibilities =
            [ ( x, y - 1 ), ( x, y + 1 ), ( x - 1, y ), ( x + 1, y ) ]

        neighbors =
            List.filter (\k -> Dict.member k dict) possibilities
    in
        List.map (\k -> ( k, 1 )) neighbors


manhattanDist2d : ( number, number ) -> ( number, number ) -> number
manhattanDist2d ( a, b ) ( x, y ) =
    (abs (a - x)) + (abs (b - y))


genTest2d : Dict.Dict ( number, number ) () -> ( number, number ) -> ( number, number ) -> Maybe (List ( number, number ))
genTest2d grid src dst =
    aStar (getNeighbors2d grid) (manhattanDist2d dst) [ src ] (Set.singleton dst)


manhattan2dTests : Test
manhattan2dTests =
    -- Currently we just test one heuristic because the heuristic should never
    -- affect correctness (so long as it's a valid a-star heuristic).
    describe "2D AStar tests (Manhattan heuristic)"
        [ test "No path returns Nothing" <|
            \() ->
                Expect.equal
                    (aStar (\x -> []) (\x -> 1) [ ( 1, 1 ) ] (Set.singleton ( 2, 2 )))
                    Nothing
        , test "Same source and dest" <|
            \() ->
                Expect.equal
                    (aStar (\x -> []) (\x -> 1) [ ( 1, 1 ) ] (Set.singleton ( 1, 1 )))
                    (Just [ ( 1, 1 ) ])
        , test "Two node grid" <|
            \() ->
                Expect.equal
                    (genTest2d grid1 ( 1, 1 ) ( 2, 1 ))
                    (Just [ ( 1, 1 ), ( 2, 1 ) ])
        , test "Grid 2" <|
            \() ->
                Expect.equal
                    (genTest2d grid2 ( 2, 1 ) ( 3, 3 ))
                    (Just [ ( 2, 1 ), ( 1, 1 ), ( 1, 2 ), ( 1, 3 ), ( 2, 3 ), ( 3, 3 ) ])
        , test "Grid 3" <|
            \() ->
                Expect.equal
                    (genTest2d grid3 ( 1, 3 ) ( 3, 3 ))
                    (Just [ ( 1, 3 ), ( 1, 2 ), ( 2, 2 ), ( 3, 2 ), ( 3, 3 ) ])
        ]



-- Functions necessary for 1D tests
-- These still use a 2D grid, but encode each point with a single number


nToRowCol : Int -> Int -> ( Int, Int )
nToRowCol width n =
    ( n // width, n % width )


rowColToMaybeN : Int -> ( Int, Int ) -> Maybe Int
rowColToMaybeN width ( row, col ) =
    if
        (row
            >= 0
        )
            && (row
                    < width
               )
            && (col
                    >= 0
               )
            && (col
                    < width
               )
    then
        Just ((row * width) + col)
    else
        Nothing


getNeighbors1d : Int -> Int -> List ( Int, number )
getNeighbors1d width n =
    let
        ( x, y ) =
            nToRowCol width n

        possibilities =
            [ ( x, y - 1 ), ( x, y + 1 ), ( x - 1, y ), ( x + 1, y ) ]

        neighbors =
            List.filterMap (rowColToMaybeN width) possibilities
    in
        List.map (\k -> ( k, 1 )) neighbors


manhattanDist1d : Int -> Int -> Int -> Int
manhattanDist1d width p1 p2 =
    let
        ( a, b ) =
            nToRowCol width p1

        ( x, y ) =
            nToRowCol width p2
    in
        (abs (a - x)) + (abs (b - y))


genTest1d : Int -> Int -> Int -> Maybe (List Int)
genTest1d width src dst =
    aStar (getNeighbors1d width) (manhattanDist1d width dst) [ src ] (Set.singleton dst)


manhattan1dTests : Test
manhattan1dTests =
    describe "1D AStar tests (Manhattan heuristic)"
        [ test "No path returns Nothing" <|
            \() ->
                Expect.equal
                    (aStar (\x -> []) (\x -> 1) [ ( 1, 1 ) ] (Set.singleton ( 2, 2 )))
                    Nothing
        , test "Same source and dest" <|
            \() ->
                Expect.equal
                    (aStar (\x -> []) (\x -> 1) [ ( 1, 1 ) ] (Set.singleton ( 1, 1 )))
                    (Just [ ( 1, 1 ) ])
        , test "2x2 test 1" <|
            \() ->
                Expect.equal
                    (genTest1d 2 0 1)
                    (Just [ 0, 1 ])
        , test "2x2 test 2" <|
            \() ->
                Expect.equal
                    (genTest1d 2 0 2)
                    (Just [ 0, 2 ])
        , test "3x3 test 1" <|
            \() ->
                Expect.equal
                    (genTest1d 3 0 2)
                    (Just [ 0, 1, 2 ])
        , test "3x3 test 2" <|
            \() ->
                Expect.equal
                    (genTest1d 3 0 6)
                    (Just [ 0, 3, 6 ])
        ]


all : Test
all =
    describe "AStar tests"
        [ manhattan2dTests, manhattan1dTests ]
