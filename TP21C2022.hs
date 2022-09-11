type Polinomio = [Float]
type Monomio = (Float, Int) 

--EJERCICIO 1
crearPolinomio :: [Float] -> Polinomio
crearPolinomio [] = []
crearPolinomio l |head l == 0 = crearPolinomio (tail l)
                              | otherwise = l

--------------------------------------

--EJERCICIO 2
grado :: Polinomio -> Int
grado [] = undefined
grado p = (length p) - 1

--------------------------------------

--EJERCICIO 3
evaluar :: Polinomio -> Float -> Float
evaluar p a  | p == [] = 0 
                     | otherwise = (head p) * (a  ^ (grado p))  + evaluar (tail  p) a 

--------------------------------------

--EJERCICIO 4
listaDeCeros :: Int -> Polinomio -> Polinomio --Crea una lista de 0s de longitud n que se agrega a otra lista 
listaDeCeros n l  | n /= 0 = listaDeCeros (n-1) (0 : l)
                              | otherwise = l 

productoPorMonomio :: Monomio -> Polinomio -> Polinomio
productoPorMonomio (a,n) [] = listaDeCeros n []
productoPorMonomio (a, n) (x:xs)  = (x * a) : (productoPorMonomio (a,n) xs)

--------------------------------------

--EJERCICIO 5
sumaCadaElementoDePolinomio :: Polinomio -> Polinomio -> Polinomio --Suma los elementos del mismo grado de 2 polinomios (AMBOS POLINOMIOS TIENEN QUE TENER EL MISMO GRADO)
sumaCadaElementoDePolinomio [] [] = []
sumaCadaElementoDePolinomio l1 l2 = (head l1 + head l2) : sumaCadaElementoDePolinomio (tail l1) (tail l2)


sumaPolinomios  :: Polinomio -> Polinomio -> Polinomio --Suma de 2 polinomios cualesquiera 
sumaPolinomios p1 p2  | grado p1 == grado p2 =  sumaCadaElementoDePolinomio p1 p2 
                                         | grado p1 > grado p2 = head p1 : sumaPolinomios (tail p1) p2
                                         | grado p2 > grado p1 = head p2 : sumaPolinomios p1 (tail p2) 

monomiosPorPolinomio :: Polinomio -> Polinomio -> [Polinomio] --Obtengo una lista con los resultados de multiplicar monomios por un polinomio 
monomiosPorPolinomio m p   | tail m == []= [(productoPorMonomio ((head m), (grado m)) p)]
                                                   | otherwise =(productoPorMonomio ((head m), (grado m)) p) : (monomiosPorPolinomio (tail m) p)

producto :: Polinomio -> Polinomio -> Polinomio
producto _ [] = []
producto [] _ = []
producto p1 p2 | tail p1 == [] = productoPorMonomio ((head p1), grado p1) p2  
                            |otherwise = sumaPolinomios (head (monomiosPorPolinomio p1 p2)) (producto (tail p1) p2)

--------------------------------------

--EJERCICIO 6 

evaluacionMultiple :: [Float] -> Polinomio -> Polinomio -> [Float] 
evaluacionMultiple [] p q = []
evaluacionMultiple l p q  | length l `mod` 2 ==  0 = evaluar q (head l) : evaluacionMultiple(tail l) p q  --Si el largo de la lista es par el primer elemento tendra indice impar
                                          |otherwise = evaluar p (head l) : evaluacionMultiple (tail l) p q 
