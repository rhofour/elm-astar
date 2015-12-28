module Test where

--import AStar exposing (aStar)
import Dict
import Set

--genGrid : List comparable -> Dict.Dict comparable ()
genGrid coords = 
    Dict.fromList (List.map (\x -> (x, ())) coords)

grid1 : Dict.Dict (number, number) ()
grid1 = 
    genGrid [(1,1), (2,1)]

grid2 : Dict.Dict (number, number) ()
grid2 = 
    genGrid [(1,1), (1,2), (2,1), (2,2)]

grid3 : Dict.Dict (number, number) ()
grid3 = 
    genGrid [(1,1), (1,2), (1,3), (2,1), (2,3), (3,1), (3,2), (3,3)]

getNeighbors : Dict.Dict (number, number) a -> (number, number) -> List ((number, number), number)
getNeighbors dict (x,y) =
    let
        possibilities = [(x, y-1), (x, y+1), (x-1, y), (x+1, y)]
        neighbors = List.filter (\k -> Dict.member k dict) possibilities
    in
       List.map (\k -> (k, 1)) neighbors

manhattanDist : (number, number) -> (number, number) -> number
manhattanDist (a,b) (x,y) = (abs (a - x)) + (abs (b - y))

--genTest : Dict.Dict (number, number) () -> (number, number) -> (number, number) -> List (number, number)
--genTest grid src dst =
--    aStar (getNeighbors grid) (manhattanDist dst) [src] (Set.singleton dst)
--
--test1 =
--    genTest grid1 (1,1) (2,1)
