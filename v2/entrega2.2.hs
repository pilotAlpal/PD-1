import Data.List
import System.Environment(getArgs)

-- ROCIO SANTOS BUITRAGO
-- OBJETIVO:  Realizar un programa que resuelva el siguiente juego con palabras:
-- DESCRIPCION: Dada una lista de palabras [p1; ... ; pn], 
--			   donde cada pi es un string -> una lista de caracteres: Palabra = String = [Char]
--
-- CONSEGUIR: una palabra destino p 
--            a base de realizar sobre pi una secuencia de transformaciones.
--


-- como introducir los args iniciales en un tipo de datos ESTADO
-- solucion pi pf = solucionAux estado
-- data Estado = {[String] String Int [TRANSF]}
-- data Estado = {[PALSINI] PALFIN NIVEL [TRANSF]}
-- StartState = Estado ["a0","b0","c0"] "hola" 0 [""] []
data Estado = Estado { startWords :: [String]
                     , endWord    :: String
                     , level      :: Int
                     , tr         :: [String]
					 , lastState  :: [Estado]
                     } deriving (Eq, Show, Read)
-- TRANSFORMACIONES
-- ----------------
-- Transformación del original SET
-- t0 : identidad    : p_1 ... p_m    :  1              :
-- t1 : reordenar    : reordenar p_i  :  sum_i (n_i!)   : rl1 i  n_i!
-- t2 : concatenar   : p_i ++ p_j     :  m(m-1)         : rl2 i  j
-- t3 : intersecar   : p_i _|_  p_j   :  m(m-1)         : rl3 i  j
-- t4 : restar       : p_i  -  p_j    :  m(m-1)         : rl4 i  j
-- t5 : desplazar    : desplazar p_i  :  m              : rl5 i
{-
transformacion({(P,tr)})  =
union ( transf1({(P,tr)}) , transf2({(P,tr)}) , transf3({(P,tr)}) , 
        transf4({(P,tr)}) , transf5({(P,tr)})  )

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

t1 p = [take (i-1) p ++ [reordena (p !! (i-1)) !! j] ++ drop i p | i<-[1..length p], j<-[0..length p]]

--el rango de i varia desde la primera palabra a la ultima
--t5 (p:ps) = [ [(take (i-1) p:ps)++(desplazar p:ps!!i+1)++(drop i length p)] | i<-[1..length (p:ps)]]
t5 p = [take (i-1) p ++ [desplazar (p !! (i-1))] ++ drop i p | i<-[1..length p]]

traza  :: (Show a2, Show a1, Show a) => a2 -> a1 -> a -> [Char]
traza num i j = "transf "++(show num)++" ( "++(show i)++" , "++(show j)++" ); "


-- FUNCIONES AUXILIARES DE LAS TRANSFORMACIONES VALIDAS
-- 1.  REORDENAR una palabra P:
-- Permutar sus caracteres. La palabra p original queda consumida, ya no esta disponible
reordena :: String -> [String]
reordena [] = []
reordena (x:[]) = [[x]]
reordena str@(x:y:[]) =  str : [reverse str]
reordena (xs) = nub $ fst $ foldl (\acc c -> (fst acc ++ (map ([c]++) $ reordena $ snd acc ++ drop ((+1) . length $ snd acc) xs), [c] ++ snd acc)) ([],"") xs

-- 2. CONCATENAR dos palabras (que quedan consumidas):
-- P++P0
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

{-
--					PARTE BASICA: 
--entrada  (convierte la entrada del enunciado en nuestra función solucion)
solucion pi pf = solucionAux pi pf 0

--solucion  (calcula la solución de un estado)
--solucionAux :: [String] -> String -> Int -> [TRANSFORMACIONES]
solucionAux :: [String] -> String -> Int -> [String]
solucionAux pi pf nivel 
	| (nivel > 5)              = "No es posible encontrar la solucion"
	| (objetivo pi pf == true) = "imprimeTraza"
	| otherwise                = transformacion pi pf nivel+1 --cuando llamo a la transformacion ya aumento el nivel

	
--transformacion (aplica una transformación a un estado)
--transformacion :: [String] -> String -> Int -> void?
transformacion pi pf nivel = (aplicarTrans) ^ (solucionAux pi pf nivel)


--objetivo  (comprueba si una pareja del estado es una solución)
objetivo pi pf = (elem pi pf)^(noRepetido estado)

--imprimeMensaje (imprime un mensaje)
--traza (genera la traza)




--VARIABLES
---------
-- m número de palabras
-- n número de letras en una palabra

-}