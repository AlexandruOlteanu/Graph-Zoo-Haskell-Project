module Algorithms where

import qualified Data.Set as S
import StandardGraph


testGraph132 :: StandardGraph Int
testGraph132 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 3)]

{-
    În etapa 1, prin graf înțelegem un graf cu reprezentare standard.
    În etapele următoare, vom experimenta și cu altă reprezentare.

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type Graph a = StandardGraph a

{-
    *** TODO ***

    Funcție generală care abstractizează BFS și DFS pornind dintr-un anumit nod,
    prin funcția de îmbinare a listelor care constituie primul parametru.
    
    Cele două liste primite ca parametru de funcția de îmbinare sunt lista
    elementelor deja aflate în structură (coadă/stivă), respectiv lista
    vecinilor nodului curent, proaspăt expandați.

    Căutarea ar trebui să țină cont de eventualele cicluri.

    Hint: Scrieți o funcție auxiliară care primește ca parametru suplimentar
    o mulțime (set) care reține nodurile vizitate până în momentul curent.
-}

function_dfs :: Ord a
          => [a]                
          -> [a]              
          -> Graph a
          -> ([a], [a])
function_dfs = \stack final_list graph ->
            let 
                current_node = (last stack)
                new_final_list = (if elem current_node final_list then 
                            final_list
                          else 
                            (final_list ++ [current_node]))
                new_stack = (take ((length stack) - 1) stack) ++ (foldl (\answer x -> 
                                            if elem x final_list then 
                                                answer  
                                            else     
                                                (x : answer) 
                            ) [] (outNeighbors current_node graph))
            in (new_stack, new_final_list)

function_bfs :: Ord a
          => [a]                
          -> [a]              
          -> Graph a  
          -> ([a], [a])
function_bfs = \queue final_list graph ->
            let
                current_node = (head queue)
                new_final_list = (if elem current_node final_list then 
                            final_list
                          else 
                            (final_list ++ [current_node]))
                new_queue = (tail queue) ++ (foldl (\answer x -> 
                                            if elem x final_list then 
                                                answer  
                                            else     
                                                (answer ++ [x]) 
                                ) [] (outNeighbors current_node graph))
            in (new_queue, new_final_list)
            
search :: Ord a
       => ([a] -> [a] -> Graph a -> ([a], [a]))
       -> [a]
       -> [a]                    
       -> Graph a              -- graful
       -> [a]                  -- lista obținută în urma parcurgerii
search function current_list final_list graph = 
    if null current_list then 
        final_list
    else
        (search function new_current_list new_final_list graph)
        where 
            answer = (function current_list final_list graph)
            new_current_list = (fst answer)
            new_final_list = (snd answer)

{-
    *** TODO ***

    Strategia BFS, derivată prin aplicarea parțială a funcției search.

    Exemple:

    > bfs 1 graph4
    [1,2,3,4]

    > bfs 4 graph4
    [4,1,2,3]
-}
bfs :: Ord a => a -> Graph a -> [a]
bfs = (\node graph -> (search function_bfs [node] [] graph)) 

{-
    *** TODO ***

    Strategia DFS, derivată prin aplicarea parțială a funcției search.

    Exemple:

    > dfs 1 graph4 
    [1,2,4,3]
    
    > dfs 4 graph4
    [4,1,2,3]
-}
dfs :: Ord a => a -> Graph a -> [a]
dfs = (\node graph -> (search function_dfs [node] [] graph)) 

{-
    *** TODO ***

    Funcția numără câte noduri intermediare expandează strategiile BFS,
    respectiv DFS, în încercarea de găsire a unei căi între un nod sursă
    și unul destinație, ținând cont de posibilitatea absenței acesteia din graf.
    Numărul exclude nodurile sursă și destinație.

    Modalitatea uzuală în Haskell de a preciza că o funcție poate să nu fie
    definită pentru anumite valori ale parametrului este constructorul de tip
    Maybe. Astfel, dacă o cale există, funcția întoarce
    Just (numărBFS, numărDFS), iar altfel, Nothing.

    Hint: funcția span.

    Exemple:

    > countIntermediate 1 3 graph4
    Just (1,2)

    Aici, bfs din nodul 1 întoarce [1,2,3,4], deci există un singur nod
    intermediar (2) între 1 și 3. dfs întoarce [1,2,4,3], deci sunt două noduri
    intermediare (2, 4) între 1 și 3.

    > countIntermediate 3 1 graph4
    Nothing

    Aici nu există cale între 3 și 1.
-}
countIntermediate :: Ord a
                  => a                 -- nodul sursă
                  -> a                 -- nodul destinație
                  -> StandardGraph a   -- graful
                  -> Maybe (Int, Int)  -- numărul de noduri expandate de BFS/DFS
countIntermediate from to graph = answer
                        where 
                            bfs_traverse = (bfs from graph)
                            dfs_traverse = (dfs from graph)
                            first_partition_bfs = (fst (span (/= to) bfs_traverse))
                            second_partition_bfs = (snd (span (/= to) bfs_traverse))
                            bfs_distance = if null second_partition_bfs then 
                                        -1
                                      else ((length first_partition_bfs) - 1)
                            first_partition_dfs = (fst (span (/= to) dfs_traverse))
                            second_partition_dfs = (snd (span (/= to) dfs_traverse))
                            dfs_distance = if null second_partition_dfs then
                                        -1
                                      else
                                          ((length first_partition_dfs) - 1)
                            answer = if (bfs_distance /= -1) && (dfs_distance /= -1) then 
                                        Just (bfs_distance, dfs_distance)
                                     else 
                                         Nothing