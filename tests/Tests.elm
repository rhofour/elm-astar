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


getNeighbors : Dict.Dict ( number, number ) a -> ( number, number ) -> List ( ( number, number ), number )
getNeighbors dict ( x, y ) =
    let
        possibilities =
            [ ( x, y - 1 ), ( x, y + 1 ), ( x - 1, y ), ( x + 1, y ) ]

        neighbors =
            List.filter (\k -> Dict.member k dict) possibilities
    in
        List.map (\k -> ( k, 1 )) neighbors


manhattanDist : ( number, number ) -> ( number, number ) -> number
manhattanDist ( a, b ) ( x, y ) =
    (abs (a - x)) + (abs (b - y))


genTest : Dict.Dict ( number, number ) () -> ( number, number ) -> ( number, number ) -> Maybe (List ( number, number ))
genTest grid src dst =
    aStar (getNeighbors grid) (manhattanDist dst) [ src ] (Set.singleton dst)


failing : Test
failing =
    test "This should fail" <| \() -> Expect.equal 1 2


all : Test
all =
    -- Currently we just test one heuristic because the heuristic should never
    -- affect correctness (so long as it's a valid a-star heuristic).
    describe "AStar tests (Manhattan heuristic)"
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
                    (genTest grid1 ( 1, 1 ) ( 2, 1 ))
                    (Just [ ( 1, 1 ), ( 2, 1 ) ])
        , test "Grid 2" <|
            \() ->
                Expect.equal
                    (genTest grid2 ( 2, 1 ) ( 3, 3 ))
                    (Just [ ( 2, 1 ), ( 1, 1 ), ( 1, 2 ), ( 1, 3 ), ( 2, 3 ), ( 3, 3 ) ])
        ]
