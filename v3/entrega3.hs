import Data.List
import System.Environment(getArgs)

-- ROCIO SANTOS BUITRAGO
-- OBJETIVO:  Realizar un programa que resuelva el siguiente juego con palabras:
-- DESCRIPCION: Dada una lista de palabras [p1; ... ; pn], 
--    donde cada pi es un string -> una lista de caracteres: Palabra = String = [Char]
--
-- CONSEGUIR: una palabra destino p 
--    a base de realizar sobre pi una secuencia de transformaciones.
--

--					PARTE BASICA: 
--entrada  (convierte la entrada del enunciado en nuestra función solucion)
--solucion :: [String]-> String -> [String]
solucion pi pf = solucionAux pi pf " X! " 0

--solucion  (calcula la solución de un estado)
--solucionAux :: [String] -> String -> Int -> [TRANSFORMACIONES]
--solucionAux :: [String] -> String -> Int -> [String]
solucionAux pi pf traza nivel 
	| (nivel > 5)              = error "No es posible encontrar la solucion : Nivel > 5"
	| (objetivo pi pf == True) = [traza++" Solucion: Nivel = "++show(nivel)]
	| otherwise                = [solucionAux (fst( j !! (i-1) )) pf (snd (j !! (i-1))) (nivel+1) | let j=(tz pi traza), i <- [1..length j]]
-- solucionAux (fst( j !! (i-1) )) pf (snd (j !! (i-1))) (nivel+1) 
--	where { j=(tz pi traza); i <- [1..length j] }
								 
--objetivo  (comprueba si una pareja del estado es una solución)
--objetivo pi pf = (elem pi pf)^(noRepetido estado)
objetivo :: [String] -> String -> Bool
objetivo pi pf = elem pf pi

-- TRAZA
-- -----
--traza1  :: (Show a2, Show a1, Show a) => a2 -> a1 -> a -> [Char]
--traza1 num i j = "transf "++(show num)++" ( "++(show i)++" , "++(show j)++" ); "
traza num i = " transf "++(show num)++" ( "++(show i)++" ); "

-- tz ["abc", "ef"] " X "
-- [[(["acb","ef"]," X  transf 1 ( 1 ); ")],[(["bac","ef"]," X  transf 1 ( 2 ); ")],
--  [(["bca","ef"]," X  transf 1 ( 3 ); ")],[(["cba","ef"]," X  transf 1 ( 4 ); ")],
--  [(["cab","ef"]," X  transf 1 ( 5 ); ")],[(["abc","fe"]," X  transf 1 ( 6 ); ")],
--  [(["abcef"]," X  transf 2 ( 1 ); ")],[(["efabc"]," X  transf 2 ( 2 ); ")],
--  [([""]," X  transf 3 ( 1 ); ")], [([""]," X  transf 3 ( 2 ); ")],
--  [(["abc"]," X  transf 4 ( 1 ); ")],[(["ef"]," X  transf 4 ( 2 ); ")],
--  [(["zab","ef"]," X  transf 5 ( 1 ); ")],[(["abc","de"]," X  transf 5 ( 2 ); ")]]
tz :: [String] -> [Char] -> [[([String], [Char])]]
tz p tr = (tz1 p tr) ++ (tz2 p tr) ++ (tz3 p tr) ++ (tz4 p tr) ++ (tz5 p tr)

-- tz1 ["abc", "ef"] " X "
-- [[(["acb","ef"]," X  transf 1 ( 1 ); ")],[(["bac","ef"]," X  transf 1 ( 2 ); ")],
--  [(["bca","ef"]," X  transf 1 ( 3 ); ")],[(["cba","ef"]," X  transf 1 ( 4 ); ")],
--  [(["cab","ef"]," X  transf 1 ( 5 ); ")],[(["abc","fe"]," X  transf 1 ( 6 ); ")]]
tz1 :: [String] -> [Char] -> [[([String], [Char])]]
tz1 p tr = [ [( j , tr++(traza 1 k))] | k<-[1..(length (t1 p))], let j=((t1 p) !! (k-1))]

-- tz2 ["abc", "hola"] " X "
-- [[(["abchola"]," X  transf 2 ( 1 ); ")],[(["holaabc"]," X  transf 2 ( 2 ); ")]]
tz2 p tr = [ [( j , tr++(traza 2 k))] | k<-[1..(length (t2 p))], let j=((t2 p) !! (k-1))]

-- tz3 ["abc", "hola"] " X "
-- [[(["a"]," X  transf 3 ( 1 ); ")],[(["a"]," X  transf 3 ( 2 ); ")]]
tz3 p tr = [ [( j , tr++(traza 3 k))] | k<-[1..(length (t3 p))], let j=((t3 p) !! (k-1))]

-- tz4 ["abc", "hola"] " X "
-- [[(["bc"]," X  transf 4 ( 1 ); ")],[(["hol"]," X  transf 4 ( 2 ); ")]]
tz4 p tr = [ [( j , tr++(traza 4 k))] | k<-[1..(length (t4 p))], let j=((t4 p) !! (k-1))]

-- tz5 ["abc", "hola"] " X "
-- [[(["zab","hola"]," X  transf 5 ( 1 ); ")],[(["abc","gnkz"]," X  transf 5 ( 2 ); ")]]
tz5 p tr = [ [( j , tr++(traza 5 k))] | k<-[1..(length (t5 p))], let j=((t5 p) !! (k-1))]

-- TRANSFORMACIONES Transf i
-- -------------------------
-- Transformación del original SET
-- t0 : identidad    : p_1 ... p_m    :  1              :
-- t1 : reordenar    : reordenar p_i  :  sum_i (n_i!)   : rl1 i  n_i!
-- t2 : concatenar   : p_i ++ p_j     :  m(m-1)         : rl2 i  j
-- t3 : intersecar   : p_i _|_  p_j   :  m(m-1)         : rl3 i  j
-- t4 : restar       : p_i  -  p_j    :  m(m-1)         : rl4 i  j
-- t5 : desplazar    : desplazar p_i  :  m              : rl5 i

{-
transformacion({(P,tr)})  =
union ( transf1({(P,tr)}) , transf2({(P,tr)}) , transf3({(P,tr)}) , transf4({(P,tr)}) , transf5({(P,tr)})  )

transf1({(P,tr)})  =
  [  ( t1( P(i) ), traza(1,i,j) )
    |  i = [1..m] , j = [1..length P(i)] , k = [1..length P(i)]  &&  j /= k ]
    where m <- card ({(P,tr)})

transf2({(P,tr)})  =
  [  ( t2( (P(i),P(j)) ), traza(2,i,j) )
    |  i = [1..m] , j = [1..m]  &&  i /= j ]
    where m <- card ({(P,tr)})

transf3({(P,tr)})  =
  [  ( t3( (P(i),P(j)) ), traza(3,i,j) )
    |  i = [1..m] , j = [1..m]  &&  i /= j ]
    where m <- card ({(P,tr)})

transf4({(P,tr)})  =
  [  ( t4( (P(i),P(j)) ), traza(4,i,j) )
    |  i = [1..m] , j = [1..m]  &&  i /= j ]
    where m <- card ({(P,tr)})
-}

-- TRANSFORMACIONES Ti
--COMPROBAR FUNCIONES TI PARA LAS LISTAS VACIAS!!!!!!!!
-- -------------------
-- t1 ["as","abc","d","fd"]
-- [["sa","abc","d","fd"],["as","acb","d","fd"],["as","bac","d","fd"],
--  ["as","bca","d","fd"],["as","cba","d","fd"],["as","cab","d","fd"],["as","abc","d","df"]]
t1 :: [String]->[[String]]
t1 p = [take (i-1) p ++ [reo (p !! (i-1)) !! (j-1)] ++ drop i p | i<-[1..length p], j<-[1..(length (reo (p !! (i-1))))]]

-- coger primer elem(i = [1..length lista]) y sgte(j=i+1..length p)
-- concatenar i j eliminar lista ++ resto elems
-- t2 ["abc","de","fg"]
-- [["abcde","fg"],["abcfg","de"],["deabc","fg"],
--  ["defg","abc"],["fgabc","de"],["fgde","abc"]]
t2 :: [String] -> [[String]]
t2 p = [[concatena (p !! (i-1)) (p !! (j-1))] ++ sinEllas p i j | i<-[1..length p], j<-[1..length p], i/=j]

-- t3 ["abcde","cde","efg"]
-- [["cde","efg"],["e","cde"],["cde","efg"],
--  ["e","abcde"],["e","cde"],["e","abcde"]]
t3 :: [String] -> [[String]]
t3 p = [[intersecar (p !! (i-1)) (p !! (j-1))] ++ sinEllas p i j | i<-[1..length p], j<-[1..length p], i/=j]

-- t4 ["abcde","cde","efg"]
-- [["ab","efg"],["abcd","cde"],["","efg"],
--  ["cd","abcde"],["fg","cde"],["fg","abcde"]]
t4 :: [String] -> [[String]]
t4 p = [[restar (p !! (i-1)) (p !! (j-1))] ++ sinEllas p i j | i<-[1..length p], j<-[1..length p], i/=j]

-- Transformación del original SET
-- t5 ["bc","ef","aa"]
-- [["ab","ef","aa"],["bc","de","aa"],["bc","ef","zz"]]
t5 :: [String] -> [[String]]
t5 p = [take (i-1) p ++ [desplazar (p !! (i-1))] ++ drop i p | i<-[1..length p]]

--sinEllas :: Eq t => [t] -> Int -> Int -> [t]
--["ho","la","ds","fd"] 1 2 -> ["ds","fd"]
sinEllas :: [String] -> Int -> Int -> [String]
sinEllas xs a b = [x|x<-xs, x/= (xs!!(a-1)), x/=(xs!!(b-1))]

--VARIABLES
------------
-- m número de palabras
-- n número de letras en una palabra

--------------------------------------------------------
-- FUNCIONES AUXILIARES DE LAS TRANSFORMACIONES VALIDAS
--------------------------------------------------------
-- 1.  REORDENAR una palabra P:
-- Permutar sus caracteres. La palabra p original queda consumida, ya no esta disponible
reo p = drop 1 (reordena p)

reordena :: String -> [String]
reordena [] = []
reordena (x:[]) = [[x]]
reordena str@(x:y:[]) =  str : [reverse str]
reordena (xs) = nub $ fst $ foldl (\acc c -> (fst acc ++ (map ([c]++) $ reordena $ snd acc ++ drop ((+1) . length $ snd acc) xs), [c] ++ snd acc)) ([],"") xs

-- 2. CONCATENAR dos palabras (que quedan consumidas):
--  P++P0
--	concatena "sub" "marino" -> "submarino"
--	concatena = (++)
concatena :: String -> String -> String
concatena p pa = p ++ pa

-- 3. INTERSECAR p con p0:
-- Quedarse con las letras de p que esten tambien en p0 (no tiene xq en la misma posicion).
-- La palabra p queda consumida pero no p0.
-- intersecar "tomate" "arboleda" -> "oae" 
-- intersecar "hola" "olla" -> "ola"  
intersecar :: String -> String -> String
intersecar [] pa = [] 
intersecar (p:ps) pa = if (elem p pa) then p:intersecar ps pa else intersecar ps pa

-- 4. RESTAR de p la palabra p0:
-- Quedarse con las letras de p que no esten en p0. 
-- La palabra p queda consumida pero p0 no.
-- restar "tomate" "arboleda" -> "tmt" 
restar :: String -> String -> String
restar [] pa = []
restar p [] = p 
restar (p:ps) pa = if (elem p pa) then restar ps pa else p:restar ps pa

-- 5. DESPLAZAR una palabra: 
-- Remplazar en ella cada caracter por su sucesor (entendiendo que el sucesor de 'z' es 'a'). 
-- La palabra original queda consumida. 
-- desplazar "tomate" -> "snlzsd"
desplazar :: String -> String
desplazar [] = []
desplazar (x:xs) = (antes x):desplazar xs

-- antes: auxiliar para desplazar: resta uno al codigo ascii
antes :: Char -> Char
antes 'a' = 'z'
antes 'A' = 'Z'
antes c = toEnum ((fromEnum c) - 1) :: Char