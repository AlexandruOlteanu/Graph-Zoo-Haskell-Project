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

f1 :: Ord a
          => [a]                -- nodul divizat
          -> [a]              -- nodurile cu care este înlocuit
          -> Graph a  -- graful obținut
          -> ([a], [a])
f1 = \l1 l2 graph ->
            let 
                node1 = (last l1)
                new_l2 = (if elem node1 l2 then 
                            l2
                          else 
                            (l2 ++ [node1]))
                new_l1 = (take ((length l1) - 1) l1)
                new_l1_1 = (new_l1 ++ (foldl (\ans x -> 
                                            if elem x l2 then 
                                                ans  
                                            else     
                                                (x : ans) 
                    ) [] (outNeighbors node1 graph)))
            in (new_l1_1, new_l2)

f2 :: Ord a
          => [a]                -- nodul divizat
          -> [a]              -- nodurile cu care este înlocuit
          -> Graph a  -- graful obținut
          -> ([a], [a])
f2 = \l1 l2 graph ->
            let
                node1 = (head l1)
                new_l2 = (if elem node1 l2 then 
                            l2
                          else 
                            (l2 ++ [node1]))
                new_l1 = (tail l1)
                new_l1_1 = (new_l1 ++ (foldl (\ans x -> 
                                            if elem x l2 then 
                                                ans  
                                            else     
                                                (ans ++ [x]) 
                    ) [] (outNeighbors node1 graph)))
            in (new_l1_1, new_l2)
            
search :: Ord a
       => ([a] -> [a] -> Graph a -> ([a], [a]))  -- funcția de îmbinare a listelor de noduri
       -> [a]
       -> [a]                    -- nodul de pornire
       -> Graph a              -- graful
       -> [a]                  -- lista obținută în urma parcurgerii
search f current_list final_list graph = 
    if null current_list then 
        final_list
    else
        (search f new_current_list new_final_list graph)
        where 
            result = (f current_list final_list graph)
            new_current_list = (fst result)
            new_final_list = (snd result)

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
bfs = (\node graph -> (search f2 [node] [] graph)) 

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
dfs = (\node graph -> (search f1 [node] [] graph)) 

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
countIntermediate from to graph = result
                        where 
                            first_list = (bfs from graph)
                            second_list = (dfs from graph)
                            p_first1 = (fst (span (/= to) first_list))
                            p_first2 = (snd (span (/= to) first_list))
                            result1 = if null p_first2 then 
                                        -1 
                                      else ((length p_first1) - 1)
                            p_second1 = (fst (span (/= to) second_list))
                            p_second2 = (snd (span (/= to) second_list))
                            result2 = if null p_second2 then
                                        -1
                                      else
                                          ((length p_second1) - 1)
                            result = if (result1 /= -1) && (result2 /= -1) then 
                                        Just (result1, result2)
                                     else 
                                         Nothing
                            
