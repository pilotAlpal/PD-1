--					PARTE BASICA CON DATA STATE: 

-- data Estado = [ [PALSINI] PALFIN NIVEL [TRANSF] [ESTADOSANTS] ]
data Estado = Estado { startWords :: [String]
                     , endWord    :: String
                     , level      :: Int
                     , traza      :: [String]					 
                    }

--[ [String] String Int [TRAZA] [Estado] ]

{-
--entrada  (convierte la entrada del enunciado en nuestra función solucion)
solucion pi pf = solucionAux pi pf 0
-- como introducir los args iniciales en un tipo de datos ESTADO
-- solucion pi pf = solucionAux estado


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
-}