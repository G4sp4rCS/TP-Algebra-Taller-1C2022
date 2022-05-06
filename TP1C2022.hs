--VERIFICA SI EL NUMERO ES PAR
esPar :: Integer -> Bool
esPar n | n == 0 = True
                 | n == 1 = False
                 | otherwise = esPar(n-2)
                                                             
                     
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
--Termina 3


--Empieza 4

largoSecuencia :: Integer -> Integer

largoSecuencia 1 = 0
largoSecuencia n | esPar n == True = 1+ largoSecuencia(n `div` 2)
 | otherwise = 1+ largoSecuencia(3 * n + 1) 
--Termina 4

--Empieza 5
secuenciaMasLargaHastaDesde :: Integer -> Integer -> Integer
secuenciaMasLargaHastaDesde m k | m == k = m 
 | largoSecuencia m  > largoSecuencia k =  secuenciaMasLargaHastaDesde m (k+1)
 |otherwise = secuenciaMasLargaHastaDesde (m-1) k 




secuenciaMasLargaHasta :: Integer -> Integer

secuenciaMasLargaHasta n = secuenciaMasLargaHastaDesde n 1 
