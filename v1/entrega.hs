import Data.List
import System.Environment(getArgs)

-- ROCIO SANTOS BUITRAGO
--OBJETIVO:  Realizar un programa que resuelva el siguiente juego con palabras:
--DESCRIPCION: Dada una lista de palabras [p1; ... ; pn], 
--			   donde cada pi es un string -> una lista de caracteres: Palabra = String = [Char]
--
--			   CONSEGUIR: una palabra destino p 
--						  a base de realizar sobre pi una secuencia de transformaciones.
--

-- 			   TRANSFORMACIONES VALIDAS:
--			 	1.  REORDENAR una palabra P:
--			  		Permutar sus caracteres. La palabra p original queda consumida, ya no esta disponible
shuffle :: String -> [String]
shuffle [] = []
shuffle (x:[]) = [[x]]
shuffle str@(x:y:[]) =  str : [reverse str]
shuffle (xs) = nub $ fst $ foldl (\acc c -> (fst acc ++ (map ([c]++) $ shuffle $ snd acc ++ drop ((+1) . length $ snd acc) xs), [c] ++ snd acc)) ([],"") xs


--				2.	CONCATENAR dos palabras (que quedan consumidas):
--					P++P0
--					concatena [] p = p
--					concatena p []  = p 
--					concatenaPalabras "super" "submarino" -> "supersubmarino"
--					concatenaPalabras = (++)
concatena :: String -> String -> String
concatena p pa = p ++ pa

--				3.	INTERSECAR p con p0:
--					Quedarse con las letras de p que esten tambien en p0 (no tiene xq en la misma posicion).
--					La palabra p queda consumida pero p0 no.
--					intersecarPalabras "tomate" "arboleda" -> "oae" 
--					intersecarPalabras "hola" "olla" -> "ola"  
intersecar :: String -> String -> String
intersecar [] pa = [] 
intersecar (p:ps) pa = if (elem p pa) then p:intersecar ps pa else intersecar ps pa

-- 				4.	Restar de p la palabra p0:
--					Quedarse con las letras de p que no esten en p0. 
--					La palabra p queda consumida pero p0 no.
--					restarPalabras "tomate" "arboleda" -> "tmt" 
restar :: String -> String -> String
restar [] pa = []
restar p [] = p 
restar (p:ps) pa = if (elem p pa) then restar ps pa else p:restar ps pa

--				5.	Desplazar una palabra: 
--					Reemplazar en ella cada caracter por su sucesor (entendiendo que el sucesor de 'z' es 'a'). 
--					La palabra original queda consumida. 
--					desplazar "tomate" -> "snlzsd"
desplazar :: String -> String
desplazar [] = []
desplazar (x:xs) = (antes x):desplazar xs

antes :: Char -> Char
antes 'a' = 'z'
antes 'A' = 'Z'
antes c = toEnum ((fromEnum c) - 1) :: Char
-- restando uno al codigo ascii


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


-- como introducir los args iniciales en un tipo de datos ESTADO
-- solucion pi pf = solucionAux estado
-- data Estado = [ [String] String Int [TRANSF] ]
-- data Estado = [ [PALSINI] PALFIN NIVEL [TRANSF] ]
