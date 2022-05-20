module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)
    deriving (Eq, Show)

-- (1, 2), (1, 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))

{-
    *** TODO ***

    Mulțimea nodurilor grafului.

    Hint: S.union
-}

-- 
nodes :: Ord a => AlgebraicGraph a -> S.Set a
nodes graph = case graph of
    -- set gol
    Empty -> S.empty
    -- cand este un singur nod
    Node x -> S.fromList (x : [])
    -- unesc recursiv listele returnate
    Overlay x y -> S.union (nodes x) (nodes y)
    Connect x y -> S.union (nodes x) (nodes y)

{-
    *** TODO ***

    Mulțimea arcelor grafului.

    Hint: S.union, S.cartesianProduct
-}

edges :: Ord a => AlgebraicGraph a -> S.Set (a, a)
edges graph = case graph of
    -- set gol
    Empty -> S.empty
    -- set gol
    Node x -> S.empty
    -- unesc recursiv listele returnate
    Overlay x y -> S.union (edges x) (edges y)
    -- produs cartezian intre nodurile din graful x si graful y
    Connect x y -> S.union (S.union (S.cartesianProduct (nodes x) (nodes y)) 
                                    (edges x))
                           (edges y)
    
    

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}

outNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
outNeighbors node graph = case graph of
    -- set gol
    Empty -> S.empty
    --set gol
    Node x -> S.empty
    -- unesc recursiv listele returnate din cele 2 grafuri x si y
    Overlay x y -> S.union (outNeighbors node x) (outNeighbors node y)
    Connect x y -> if elem node (S.toList (nodes x)) 
                   then (nodes y) 
                   else outNeighbors node x

{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}

inNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
inNeighbors node graph = case graph of
    -- set gol
    Empty -> S.empty
    -- set gol
    Node x -> S.empty
    -- unesc recursiv listele returnate din cele 2 grafuri x si y
    Overlay x y -> S.union (inNeighbors node x) (inNeighbors node y)
    Connect x y -> if elem node (S.toList (nodes y)) 
                   then S.union (nodes x) (S.union (inNeighbors node x) (inNeighbors node y)) 
                   else inNeighbors node x

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, se întoarce același graf.

    Hint: Definiți o funcție recursivă locală (de exemplu, în where),
    care să primească drept parametri doar entități variabile de la un apel
    recursiv la altul, astfel încât să nu copiați la fiecare apel parametrii
    nemodificați. De exemplu, parametrul node nu se modifică, în timp ce
    parametrul graph se modifică.

-}

removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
-- set gol
removeNode node Empty = Empty
-- daca gasesc nodul ce trebuie sters, il sterg; altfel returnez graful initial
removeNode node (Node x) = if x == node then Empty else Node x
-- unesc recursiv listele returnate din cele 2 grafuri x si y
removeNode node (Overlay x y) = (Overlay (removeNode node x) (removeNode node y))
removeNode node (Connect x y) = (Connect (removeNode node x) (removeNode node y))



{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.
    
    Hint: Funcție recursivă locală, ca la removeNode.
-}

splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
splitNode old news Empty = Empty
splitNode old news (Node x) = if x == old then Empty else Node x
splitNode old news (Overlay x y) = (Overlay (splitNode old news x) (splitNode old news y))
splitNode old news (Connect x y) = (Connect (splitNode old news x) (splitNode old news y))

{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Hint: Funcție recursivă locală, ca la removeNode.
-}

mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
-- set gol
mergeNodes prop node Empty = Empty
--daca gasesc nodul cu proprietatea data atunci formez graful din nodul respectiv
-- altfel returnez graful  initial
mergeNodes prop node (Node x) = if prop x then Node node else Node x
-- unesc recursiv listele returnate din cele 2 grafuri x si y
mergeNodes prop node (Overlay x y) = (Overlay (mergeNodes prop node x) (mergeNodes prop node y))
mergeNodes prop node (Connect x y) = (Connect (mergeNodes prop node x) (mergeNodes prop node y))
