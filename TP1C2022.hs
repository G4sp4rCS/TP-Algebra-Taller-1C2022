esPar :: Integer -> Bool
esPar n | n == 0 = True
                 | n == 1 = False
                 | otherwise = esPar(n-2)

hacerCero :: Integer -> Integer              
hacerCero x | x == 0 = 0 
                           | otherwise = hacerCero (x-1)

--sequenciaCollatz :: Integer ->  Integer

--sequenciaCollatz n | n == 1 = 1
                                            -- | esPar n == True =  sequenciaCollatz(n `div` 2)
                                            -- | otherwise = sequenciaCollatz(3 * n + 1 )
                     
                    
sequenciaCollatz2 n       | esPar n = n `div` 2
                                                      | otherwise = 3 * n + 1
                     
                     
                
kEsimoCollatz :: Integer -> Integer -> Integer

kEsimoCollatz n 0 =  n
kEsimoCollatz 1 k = 1
kEsimoCollatz  n k    | esPar n == True = kEsimoCollatz(n `div` 2) (k-1)
                                            | otherwise = kEsimoCollatz(3 * n + 1) (k-1)                                         
                              


satisfaceCollatz :: Integer -> Integer -> Bool

satisfaceCollatz 1 1 = False
satisfaceCollatz 1 2 = False
satisfaceCollatz 1 3 = False
satisfaceCollatz n m  | kEsimoCollatz n m == 1 = True
				         | otherwise = False
						 
--Termina 1
--Empieza 2
