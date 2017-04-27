--					PARTE BASICA SIN DATA STATE: 
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
-- data Estado = [ [String] String Int [TRAZA] [Estado] ]
-- data Estado = [ [PALSINI] PALFIN NIVEL [TRANSF] [ESTADOSANTS] ]
