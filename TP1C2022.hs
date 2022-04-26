--VERIFICA SI EL NUMERO ES PAR
esPar :: Integer -> Bool
esPar n | n == 0 = True
                 | n == 1 = False
                 | otherwise = esPar(n-2)


--VERIFICA SI CUMPLE LA CONJETURA DE COLLATZ
sequenciaCollatz :: Integer ->  Bool

sequenciaCollatz n | n == 1 = True
                                            | esPar n == True =  sequenciaCollatz(n `div` 2)
                                            | esPar n == False = sequenciaCollatz(3 * n + 1 )
				    | otherwise = False
                     
                                        
                     
 --CALCULA EL TERMINO k-ESIMO DE LA SUCESION DE COLLATZ DE UN NUMERO              
kEsimoCollatz :: Integer -> Integer -> Integer

kEsimoCollatz n 0 =  n 
kEsimoCollatz 1 k = 1
kEsimoCollatz  n k    | esPar n == True = kEsimoCollatz(n `div` 2) (k-1)
                                            | otherwise = kEsimoCollatz(3 * n + 1) (k-1)                                         
                              

--EJERCICIO 1

satisfaceCollatz :: Integer -> Integer -> Bool

satisfaceCollatz n m  | kEsimoCollatz n m == 1 = True
				         | otherwise = False
						 
--Termina 1

--Empieza 2

--EJERCICIO 2

satisfaceCollatzHasta :: Integer -> Integer -> Bool
satisfaceCollatzHasta 0 m = True
satisfaceCollatzHasta n m|satisfaceCollatz n m == True = satisfaceCollatzHasta (n-1) m 
                                                            | otherwise = False
															

--Termina 2

--Empieza 3
 
cantidadTerminosPares :: Integer -> Integer
cantidadTerminosPares 1 = 0
cantidadTerminosPares n | esPar n == True = 1+ cantidadTerminosPares (n `div` 2)
                                                         | otherwise = cantidadTerminosPares (3 * n + 1) 
                                                    													 
--TERMINAR 3


--Empieza 4

--largoSecuencia :: Integer -> Integer

--largoSecuencia 1 = 0
--largoSecuencia n | kEsimoCollatz n  == 1 = n 
                                        

														 
														 
														 
														 





