--Ejercicio 1

--1 map (^2) [0..50]
--2 reverse (zip [0..50] (map (^2) [0..50]))

f1 x = x * abs (sin x) 
--3 sum (map f1[1..100])

--4 take 50 (iterate (*3) 1 )

--5 takeWhile ( < 10^100)  (iterate (*3) 1 )
-- map (`mod`100) 
-- filter ( == 67) 
-- length

--6 sum (filter (\n ->(n `mod` 3 == 0) || (n `mod` 5 == 0)) [1..1000])
--7 Formulas de progresiones aritmeticas lolXD
f2 x y = (map (^x) y) 
--8 
-- map (`f2` [1..20]) [1..10]



--Ejercicio 3

--1 Lista de divisores de un numero
divisible x y = if (x `mod` y == 0) then True
else False

divisores x = filter (divisible x) [1..x]

--2 Lista con los numeros entre 19 y 50 cada uno emparejado con sus divisores
divisores1 x = filter (divisible x) [1..x-1]

--zip [19..50] (map divisores1 [19..50])

--3 Numero primo
primo x = if ( length (divisores1 x) == 1) then True
else False

--4 Lista infinita de primos
--filter primo [1..]

--5 La lista de los primos menores que 1000
--filter primo [1..1000]

--6 La cantidad de nueros primos que hay entre 200 y 500
--length (filter primo [200..500])

--7 El primer primo mayor que 6923

--head (filter primo (filter (>6923) [1..]))


