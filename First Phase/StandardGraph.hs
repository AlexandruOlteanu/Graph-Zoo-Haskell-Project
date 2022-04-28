{-# LANGUAGE TupleSections #-}
module StandardGraph where

import qualified Data.Set as S

{-
    Graf ORIENTAT cu noduri de tipul a, reprezentat prin mulțimile (set)
    de noduri și de arce.

    Mulțimile sunt utile pentru că gestionează duplicatele și permit
    testarea egalității a două grafuri fără a ține cont de ordinea nodurilor
    și a arcelor.

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type StandardGraph a = (S.Set a, S.Set (a, a))

{-
    *** TODO ***

    Construiește un graf pe baza listelor de noduri și de arce.

    Hint: S.fromList.

    Constrângerea (Ord a) afirmă că valorile tipului a trebuie să fie
    ordonabile, lucru necesar pentru reprezentarea internă a mulțimilor.
    Este doar un detaliu, cu care nu veți opera explicit în această etapă.
    Veți întâlni această constrângere și în tipurile funcțiilor de mai jos.
-}
fromComponents :: Ord a
               => [a]              -- lista nodurilor
               -> [(a, a)]         -- lista arcelor
               -> StandardGraph a  -- graful construit
fromComponents ns es = 
                (S.fromList ns, S.fromList es) 

{-
    *** TODO ***

    Mulțimea nodurilor grafului.
-}
nodes :: StandardGraph a -> S.Set a
nodes = (\graph -> (fst graph))


{-
    *** TODO ***

    Mulțimea arcelor grafului.
-}
edges :: StandardGraph a -> S.Set (a, a)
edges = (\graph -> (snd graph))

{-
    Exemple de grafuri
-}
graph1 :: StandardGraph Int
graph1 = fromComponents [1, 2, 3, 3, 4] [(1, 2), (1, 3), (1, 2)]

graph2 :: StandardGraph Int
graph2 = fromComponents [4, 3, 3, 2, 1] [(1, 3), (1, 3), (1, 2)]

graph3 :: StandardGraph Int
graph3 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 3), (1, 3)]

graph4 :: StandardGraph Int
graph4 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 4), (1, 3)]

shouldBeTrue :: Bool
shouldBeTrue = graph1 == graph2


extract_neighbours :: Ord a 
                        => a
                        -> S.Set (a, a)
                        -> S.Set a     
extract_neighbours wanted_node graph_edges = S.fromList (foldl (\answer pair -> (if (fst pair) == wanted_node then 
                                                            ((snd pair) : answer)
                                                        else answer)) [] graph_edges)

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    Exemplu:

    > outNeighbors 1 graph3
    fromList [2,3,4]
-}
outNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
outNeighbors wanted_node graph = extract_neighbours wanted_node (edges graph)

{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    Exemplu:

    > inNeighbors 1 graph3 
    fromList [4]
-}
inNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
inNeighbors wanted_node graph = (extract_neighbours wanted_node graph_edges)
                         where 
                             graph_edges = S.fromList (map (\pair -> (snd pair, fst pair)) (S.toList (edges graph)))

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, întoarce același graf.

    Exemplu:

    > removeNode 1 graph3
    (fromList [2,3,4],fromList [(2,3)])
-}
removeNode :: Ord a => a -> StandardGraph a -> StandardGraph a
removeNode wanted_node graph = (fromComponents nodes_after_removal edges_after_removal)
                        where 
                            nodes_after_removal = foldl (\answer current_node -> (if current_node == wanted_node then 
                                                            answer 
                                                          else 
                                                              (current_node : answer)
                                                )) [] (nodes graph)
                            edges_after_removal = foldl (\answer pair -> (if ((fst pair) == wanted_node 
                                                                    || (snd pair) == wanted_node) then 
                                                            answer 
                                                           else 
                                                               (pair : answer)
                                                            )) [] (edges graph)
                            

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.

    Exemplu:

    > splitNode 2 [5,6] graph3
    (fromList [1,3,4,5,6],fromList [(1,3),(1,4),(1,5),(1,6),(4,1),(5,3),(6,3)])
-}
splitNode :: Ord a
          => a                -- nodul divizat
          -> [a]              -- nodurile cu care este înlocuit
          -> StandardGraph a  -- graful existent
          -> StandardGraph a  -- graful obținut
splitNode old new graph = (fromComponents new_nodes new_edges)
                         where
                            new_graph = removeNode old graph 
                            edges_before_add = (S.toList (edges new_graph))
                            edge_list_in = (map (\new_node -> 
                                             (foldl (\answer node -> [(node, new_node)] ++ answer) [] (inNeighbors old graph))
                                        ) new)
                            edge_list_out = (map (\new_node -> 
                                             (foldl (\answer node -> [(new_node, node)] ++ answer) [] (outNeighbors old graph))
                                        ) new)
                            new_nodes = (foldl (\ans x -> (x : ans)) (S.toList (nodes new_graph)) new)
                            new_edges = edges_before_add ++ (foldl (\answer new_edge -> (new_edge ++ answer)) [] edge_list_out)
                                                          ++ (foldl (\answer new_edge -> (new_edge ++ answer)) [] edge_list_in)
                            
{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Exemplu:

    > mergeNodes even 5 graph3
    (fromList [1,3,5],fromList [(1,3),(1,5),(5,1),(5,3)])
-}
mergeNodes :: Ord a
           => (a -> Bool)      -- proprietatea îndeplinită de nodurile îmbinate
           -> a                -- noul nod
           -> StandardGraph a  -- graful existent
           -> StandardGraph a  -- graful obținut
mergeNodes prop node graph = (fromComponents new_nodes new_edges)
                            where 
                                initial_nodes = (nodes graph)
                                new_graph = (foldl (\answer x -> (if prop x then 
                                                                (splitNode x [node] answer)
                                                               else 
                                                                answer)) graph initial_nodes)
                                new_edges = S.toList (edges new_graph)
                                new_nodes = S.toList (nodes new_graph)