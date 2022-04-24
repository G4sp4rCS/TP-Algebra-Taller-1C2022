esPar :: Integer -> Bool
esPar n | n == 0 = True
                 | n == 1 = False
                 | otherwise = esPar(n-2)

hacerCero :: Integer -> Integer              
hacerCero x | x == 0 = 0 
                           | otherwise = hacerCero (x-1)

sequenciaCollatz :: Integer ->  Bool

sequenciaCollatz n | n == 1 = True
                                            | esPar n == True =  sequenciaCollatz(n `div` 2)
                                            | esPar n == False = sequenciaCollatz(3 * n + 1 )
				    | otherwise = False
                     
                    
sequenciaCollatz2 n       | esPar n = n `div` 2
                                                      | otherwise = 3 * n + 1
                     
                     
                
kEsimoCollatz :: Integer -> Integer -> Integer

kEsimoCollatz n 0 =  n
kEsimoCollatz 1 k = 1
kEsimoCollatz  n k    | esPar n == True = kEsimoCollatz(n `div` 2) (k-1)
                                            | otherwise = kEsimoCollatz(3 * n + 1) (k-1)                                         
                              


satisfaceCollatz :: Integer -> Integer -> Bool

--satisfaceCollatz 1 1 = False
--satisfaceCollatz 1 2 = False
--satisfaceCollatz 1 3 = False
satisfaceCollatz n m  | kEsimoCollatz n m == 1 = True
				         | otherwise = False
						 
--Termina 1

--Empieza 2

satisfaceCollatzHasta :: Integer -> Integer -> Bool
satisfaceCollatzHasta 0 m = True
satisfaceCollatzHasta n m|satisfaceCollatz n m == True = satisfaceCollatzHasta (n-1) m 
                                                            | otherwise = False
															

--Termina 2

--Empieza 3

--kEsimoCollatzPar :: Integer -> Integer -> Integer

--kEsimoCollatzPar n 0 =  n
--kEsimoCollatzPar 1 k = 1
--kEsimoCollatzPar  n k  | esPar n == True = kEsimoCollatz(n `div` 2) (k-1)
                                               --   | otherwise = kEsimoCollatz(3 * n + 1) (k-1) 



--cantidadTerminosPares :: Integer -> Integer
--cantidadTerminosPares n | sequenciaCollatz n == False = 0
                                                        -- |  esPar n == True = 
														 
--TERMINAR 3


--Empieza 4

largoSecuencia :: Integer -> Integer

largoSecuencia 1 = 0
largoSecuencia n | kEsimoCollatz n n == 1 = n 
