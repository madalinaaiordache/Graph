module Modular where

import Data.List
import Data.Function (on)
import qualified Data.Set as S
import StandardGraph

type Graph a = StandardGraph a

{-
    O partiție este o mulțime de submulțimi ale unei alte mulțimi, disjuncte
    (fără elemente comune) și care împreună conțin toate elementele originale.
    
    De exemplu, pentru mulțimea [1,2,3], o posibilă partiție este [[1], [2,3]].

    Va fi folosită în etapa 3.
-}
type Partition a = S.Set (S.Set a)

{-
    *** TODO ***

    Aplică o funcție pe fiecare element al unei liste, însă doar pe unul singur
    la un moment dat, păstrându-le pe celalte nemodificate. Prin urmare, pentru
    fiecare element din lista inițială rezultă câte o listă în lista finală,
    aferentă modificării doar a acelui element.

    Exemplu:

    > mapSingle (+10) [1,2,3]
    [[11,2,3],[1,12,3],[1,2,13]]
-}

-- pe exemplul dat, cum se obtine [11,2,3]

-- aux 1 => splitAt index xs ([], [1,2,3]
aux1 index xs = fst (splitAt index xs)
--aux2 => splitAt (index + 1) xs ([1], [2,3])
aux2 index xs = snd (splitAt (index + 1) xs)
-- aux3 aplica functia pe elementul de la indexul respectiv
aux3 f index xs = f (head (snd (splitAt index xs)))
-- concatenez listele obtinute din functiile auxiliare anterioare
concatt f x xs = (aux1 x xs) ++ [aux3 f x xs] ++ (aux2 x xs)


-- mapez functiile anterioare iterand prin lista xs
mapSingle :: (a -> a) -> [a] -> [[a]]
mapSingle f xs = map (\x -> concatt f x xs) [0,1..(length xs - 1)]


{-
    *** TODO ***

    Determină lista tuturor partițiilor unei liste. Deși mai sus tipul
    Partition a este definit utilizând mulțimi, aici utilizăm liste,
    pentru simplitate.

    Dacă vi se pare greu de urmărit tipul întors de funcție, cu 3 niveluri
    ale constructorului de tip listă, gândiți-vă așa:
    - avem nevoie de un nivel pentru o submulțime
    - încă un nivel pentru o partiție, care este o mulțime de submulțimi
    - încă un nivel pentru mulțimea tuturor partițiilor.

    Hint: Folosiți list comprehensions pentru a răspunde la întrebarea:
    dacă am obținut o partiție a restului listei, cum obținem o partiție
    a întregii liste, care include capul? (folosiți și mapSingle)

    Exemple:

    > partitions [2,3]
    [[[2],[3]],[[2,3]]]

    > partitions [1,2,3]
    [[[1],[2],[3]],[[1,2],[3]],[[2],[1,3]],[[1],[2,3]],[[1,2,3]]]
-}

-- functie care imi intoarce toate posibilele aplicari ale functiei 
-- partition asupra unui element din lista
possibilities :: (a->a) -> [a] -> [[a]]
possibilities _ [] = []
possibilities f (x:xs) = (f x:xs) : map (x:) (possibilities f xs) 

-- pentru (x:xs) obtinem doua feluri de partitii
-- 1. cele in care x face parte din propria partitie 
--                     (map ([b] :) (partitions bs))
-- 2. cele in care x apartine aceleiasi partii ca si succesorul lui
-- din lista initiala (le obtinem lipind x in fata fiecarui prim element din partitiile lui xs)
--                     [possibilities (b:) bp | bp <- partitions bs]
-- pentru o lista goala, obtinem o partitie goala

partitions :: [a] -> [[[a]]]
partitions [] = [[]]
partitions (b:bs) = (map ([b] :) (partitions bs))
               ++ concat [possibilities (b:) bp | bp <- partitions bs]