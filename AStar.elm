module AStar where

import Set exposing (Set(..))
import PairingHeap as PH

-- Returns a shortest path from a source node to a goal node.
-- If a path does not exist an empty list is returned.
--aStar : (comparable -> List (comparable, number))
--    -> (comparable -> number)
--    -> List comparable
--    -> Set comparable
--    -> List comparable
aStar getNeighbors heuristic sources goals =
    let
        aStar' : PH.PairingHeap number (number, comparable, List comparable) -> Set comparable -> List comparable
        aStar' open closed = 
            case PH.findMin open of
                Nothing ->
                    []

                Just (cost, (dist, nextNode, partPath)) ->
                    if Set.member (Debug.log "nextNode" nextNode) goals
                       then
                          partPath
                       else
                          let
                              closed' : Set comparable
                              closed' = Set.insert nextNode closed

                              neighbors : List comparable
                              neighbors = List.filter (\x -> not (Set.member x closed')) (getNeighbors nextNode)

                              open' : PH.PairingHeap number (number, comparable, List comparable)
                              open' = List.foldl (\(x, d) -> 
                                  PH.insert (dist + d + heuristic x, (dist + d, x, x::partPath))) open neighbors
                          in
                          aStar' open' closed'
    in
       aStar' (PH.fromList (List.map (\x -> (heuristic x, (0, x, [x]))) sources)) Set.empty
