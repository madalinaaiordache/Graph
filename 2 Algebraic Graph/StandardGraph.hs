{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use section" #-}
{-# HLINT ignore "Use concatMap" #-}
{-# HLINT ignore "Use infix" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
module StandardGraph where

import qualified Data.Set as S
import qualified Data.Tuple as S
import Control.Monad.RWS (Any(Any))
import qualified Data.Semigroup as S

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

    Set.fromList list -- returns a set created from a list (duplicates are removed)

    Constrângerea (Ord a) afirmă că valorile tipului a trebuie să fie
    ordonabile, lucru necesar pentru reprezentarea internă a mulțimilor.
    Este doar un detaliu, cu care nu veți opera explicit în această etapă.
    Veți întâlni această constrângere și în tipurile funcțiilor de mai jos.
-}


fromComponents :: Ord a
               => [a]              -- lista nodurilor
               -> [(a, a)]         -- lista arcelor
               -> StandardGraph a  -- graful construit
fromComponents ns es  =  (S.fromList ns, S.fromList es)
    


{-
    *** TODO ***

    Mulțimea nodurilor grafului.
-}
nodes :: StandardGraph a -> S.Set a
nodes = S.fst 


{-
    *** TODO ***

    Mulțimea arcelor grafului.
-}
edges :: StandardGraph a -> S.Set (a, a)
edges = S.snd

{-
EXEMPLU:
*TestGraph> fromComponents [1, 2, 3, 3, 4] [(1, 2), (1, 3), (1, 2)]
(fromList [1,2,3,4],fromList [(1,2),(1,3)])

*TestGraph> nodes (S.fromList [1,2,3,4],S.fromList [(1,2),(1,3)])
fromList [1,2,3,4]

*TestGraph> edges (S.fromList [1,2,3,4],S.fromList [(1,2),(1,3)])
fromList [(1,2),(1,3)]

OBS: nodes, edges -> definedfunctions without explicit parameter
-}


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

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    Exemplu:

    > outNeighbors 1 graph3
    fromList [2,3,4]


    idee: ma duc in multimea arcelor grafului = lista de tupluri pe care o transform astfel:
    fac o noua lista ce contine doar tuplurile cu primul element = nodul curent dinspre care
    pleaca arce catre alte noduri si o transform pe aceasta intr-o lista cu elemente obisnuite, numere
   ex: 

   S.filter ((==1).fst) (S.fromList [(1, 2), (2, 3), (3, 5), (1, 6), (6, 3), (6, 7), (7, 5)])
   => fromList [(1,2),(1,6)]

   concat (S.map (\(x,y) -> [x,y])(S.fromList [(1, 2), (1, 6)]))
    => [1,2,1,6]

   Pentru a scapa de elementele duplicate, creez un set din lista obtinuta
       => S.fromList (concat  (S.map (\(x,y) -> [x,y]) (S.filter ((==1).fst) (S.fromList [(1,2),(1,6)]))))
       fromList [1,2,6]

    Cu S.delete sterg nodul dorit si obtin rezultatul cautat
-}

outNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
outNeighbors node graph = S.delete node (S.fromList (concat  (S.map (\(x,y) -> [x,y]) (S.filter ((==node).fst) (edges graph)))))



    
    

{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    Exemplu:

    > inNeighbors 1 graph3 
    fromList [4]


    idee: ma duc in multimea arcelor grafului = lista de tupluri pe care o transform astfel:
    fac o noua lista ce contine doar tuplurile cu al doilea element = nodul curent inspre care
    pleaca arce din alte noduri si o transform pe aceasta intr-o lista cu elemente obosnuite, numere
   ex: 

   S.filter ((==1).snd) (S.fromList [(1, 2), (1, 4), (4, 1), (2, 3), (1, 3)])
   => fromList [(4,1)]
   In continuare,  analog precum la outNeighbours.
-}


inNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
inNeighbors node graph = S.delete node (S.fromList (concat  (S.map (\(x,y) -> [x,y]) (S.filter ((==node).snd) (edges graph)))))



{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, întoarce același graf.

    Exemplu:
    
    > removeNode 1 graph3
    (fromList [2,3,4],fromList [(2,3)])
-}

-- functie auxiliara care imi sterge un element dintr-o lista
-- o folosesc pentru a sterge din lista nodurilor nodul ce trebuie eliminat din graf

remove :: Eq a => a -> [a] -> [a]
remove element list = filter (\e -> e/=element) list

-- functie auxiliara care imi elimina arcele in care apare nodul
-- filtrez lista de tupluri a.i. primul, respectiv, al doilea element din fiecare
-- tuplu sa fie diferit de nodul ce trebuie sters

specialFilter :: Eq a => a -> StandardGraph a -> [(a, a)]
specialFilter node graph = filter ((/=node).snd) (filter ((/=node).fst) (S.toList(edges graph)))

-- folosesc functia fromComponents pentru a crea noul graf
-- ce contine noile liste de noduri si de arce

removeNode :: Ord a => a -> StandardGraph a -> StandardGraph a
removeNode node graph = fromComponents (remove node (S.toList (nodes graph))) (specialFilter node graph)



{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.

    Exemplu:
    
    graph3 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 3), (1, 3)]

    > splitNode 2 [5,6] graph3
    (fromList [1,3,4,5,6],fromList [(1,3),(1,4),(1,5),(1,6),(4,1),(5,3),(6,3)])
-}


-- functie auxiliara care inlocuieste nodul vechi cu nodurile noi in lista nodurilor
-- (++) ((++) (S.toList(fst (S.split 3 (S.fromList [1,2,3,4])))) [5,6]) (S.toList(snd (S.split 3 (S.fromList [1,2,3,4]))))
-- =>[1,2,5,6,4]
-- iau lista arcelor si o splituiesc la nodul de eliminat -> obtin doua sub-seturi intre care concatenez nodurile noi

newnodes :: Ord a => a -> [a] -> StandardGraph a -> [a]
newnodes old news graph = (++) 
                          ((++) (S.toList(fst (S.split old (nodes graph))))
                                 news) 
                          (S.toList(snd (S.split old (nodes graph))))




{-
    *TestGraph> fun1 (2, [5,6,7])
    [(2,5),(2,6),(2,7)]
    *TestGraph> fun2 ([5,6,7], 2)
    [(5,2),(6,2),(7,2)]

    fun1, fun2 = Functii ajutatoare care imi genereaza tupluri
-}

fun1 :: (a, [a]) -> [(a, a)]
fun1 (i, xs) = map (\x -> (i, x)) xs
--SAU fun1 (i,xs) = [(i,x) | x <- xs] list comprehension

fun2 :: ([a], a) -> [(a, a)]
fun2 (xs, i) = map (\x -> (x, i)) xs



{- 
[2 3] = lista cu nodurile din toate tuplurile ce contin nodul old, dar fara nodul old
Deci, aplic functiile func1 sau func2, in functie de caz, pe fiecare elemnt din aceasta
lista, generand toate tuplurile necesare intocmirii listei arcelor din noul graf.

*TestGraph> map (\x -> fun1 (x, [5,6,7])) [2,3]
[[(2,5),(2,6),(2,7)],[(3,5),(3,6),(3,7)]]
*TestGraph> concat (map (\x -> fun1 (x, [5,6,7])) [2,3])
[(2,5),(2,6),(2,7),(3,5),(3,6),(3,7)]
-}


delete :: Eq b => b -> [(b, b)] -> [(b, b)]
delete y xs = filter (not . ((==) y) . fst) (filter (not . ((==) y) . snd) xs)


-- functie auxiliara care concateneaza toate tuplurile ce contin nodul old intr-o lista
-- filtrez tuplurile verificand daca primul sau al doilea element din acestea este egal 
-- cu nodul old si le concatenez pentru a obtine o lista
createList :: Eq b => b -> [(b, b)] -> [(b, b)]
createList y xs = (++) (filter (\x -> (snd x) == y) xs) (filter (\x -> (fst x) == y) xs)

{-
*TestGraph> selectValue1 1 [(1,2),(1,3)]
[2,3]
*TestGraph> selectValue2 2 [(1,2),(1,3)]
[1]
-}

-- de apelat pe createList pentru a-mi forma o lista 
-- cu nodurile din toate tuplurile ce contin nodul old, dar fara nodul old
selectValue1 :: Eq a1 => a1 -> [(a1, a2)] -> [a2]
selectValue1 a xs = [ y | (x,y) <- xs, x == a]

selectValue2 :: Eq a1 => a1 -> [(a2, a1)] -> [a2]
selectValue2 a xs  = [ x | (x,y) <- xs, y == a]
                                
-- functie auxiliara care inlocuieste arcele vechi cu cele noi
newedges :: Ord a => a -> [a] -> StandardGraph a-> [(a, a)]
newedges old news graph = S.toList (S.union 
                                        (S.fromList (delete old (S.toList (edges graph)))) 
                                        (S.union 
                                                (S.fromList (concat (map (\x -> fun2 (news, x)) 
                                                            (selectValue1 old (createList old (S.toList (edges graph))))))) 
                                                (S.fromList (concat (map (\x -> fun1 (x, news)) 
                                                            (selectValue2 old (createList old (S.toList (edges graph)))))))))
                                

-- daca lista nodurilor cu care trebuie inlocuit nodul divizat este nula
-- practic trebuie sa elemin nodul old din graf si apelez removeNode
-- altfel formez noul graf obtinut cu ajutorul functiilor auxiliare

splitNode :: Ord a
          => a                -- nodul divizat
          -> [a]              -- nodurile cu care este înlocuit
          -> StandardGraph a  -- graful existent
          -> StandardGraph a  -- graful obținut


splitNode old news graph = if null news 
                           then removeNode old graph 
                           else fromComponents (newnodes old news graph)(newedges old news graph)



{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Exemplu:

    > mergeNodes even 5 graph3
    (fromList [1,3,5],fromList [(1,3),(1,5),(5,1),(5,3)])

-}


-- functie ce creeaza o lista cu un sg elem dintr-un nr
-- fiindca ma nevoie de o lista ce contine doar noul nod
convertToList :: a -> [a]
convertToList n = [n]

convertToPair :: [(a, b)] -> (a, b)
convertToPair [(a, b)] = (a, b)


-- lista nodurilor ce trebuie sterse (nodurile ce vor fi imbinate)
deletednodes :: Ord a => (a -> Bool) -> p -> StandardGraph a -> S.Set a
deletednodes prop node graph = S.fromList (filter prop (S.toList (nodes graph)))


-- functie ce imi genereaaza noua lista a nodurilor 
-- cu filter prop (nodes graph) ma duc in proprietatea data si extrag lista nodurilor ce trebuie imbinate
-- fac diferenta intre multimea veche a nodurilor si nodurile ce trebuie imbinate la care adaug noul nod
newnodes2 :: Ord a => (a -> Bool) -> a -> StandardGraph a -> [a]
newnodes2 prop node graph = S.toList (S.union (S.difference (nodes graph) 
                                              (deletednodes prop node graph))
                                     (S.fromList (convertToList node)))

-- functie ce imi genereaza lista cu tupluri care nu contin noduri de sters [(4,5)]
keepTuple :: Ord a => (a -> Bool) -> p -> StandardGraph a -> S.Set (a, a)
keepTuple prop node graph = S.fromList (filter (\(x, y) -> notElem x (deletednodes prop node graph) && 
                                                           notElem y (deletednodes prop node graph)) 
                                        (S.toList (edges graph)))

-- formez lista cu elemente din edges care vor fi eliminate --> de sters in totalitate fromList [(1,2),(1,3),(2,3)]
deleteTuple :: Ord a => (a -> Bool) -> p -> StandardGraph a -> S.Set (a, a)
deleteTuple prop node graph = S.fromList (filter (\(x, y) -> elem x (deletednodes prop node graph) && 
                                                             elem y (deletednodes prop node graph)) 
                                         (S.toList (edges graph)))

-- formez lista cu tupluri din edges care trebuie modificate [(3,4),(3,5),(5,1)]
modifyTuple :: Ord a => (a -> Bool) -> p -> StandardGraph a -> S.Set (a, a)
modifyTuple prop node graph = S.difference (S.difference (edges graph) 
                                                         (keepTuple prop node graph)) 
                                            (deleteTuple prop node graph)


-- imbin seturile sa dea noua lista de edges
newedges2 :: Ord a => (a -> Bool) -> a -> StandardGraph a -> [(a, a)]
newedges2 prop node graph = let ll = convertToList node
                                l7 = deletednodes prop node graph
                                l1 = S.toList (deleteTuple prop node graph); 
                                l2 = (S.toList (modifyTuple prop node graph));
                                l3 = S.fromList [(x, y) | (x,y) <- l1, x <- ll, y <- ll];
                                l4 = S.fromList [(x, y) | (x,y) <- (filter (\(x, y) -> elem x l7) l2), x <- ll];
                                l5 = S.fromList [(x, y) | (x,y) <- (filter (\(x, y) -> elem y l7) l2), y <- ll];
                            in  S.toList (S.union (S.union (S.union l3 l4) l5) (keepTuple prop node graph))                            


-- daca nu este niciun nod de imbinat, programul nu face nimic, returneaza tot graful initial
-- altfel returneaza graful format din noua lista de noduri si noua lista de arce

mergeNodes :: Ord a
           => (a -> Bool)      -- proprietatea îndeplinită de nodurile îmbinate
           -> a                -- noul nod
           -> StandardGraph a  -- graful existent
           -> StandardGraph a  -- graful obținut
mergeNodes prop node graph = if null (deletednodes prop node graph) 
                             then graph 
                             else fromComponents (newnodes2 prop node graph)(newedges2 prop node graph)