--Ejercicio 3 
digitos:: Int -> Int
digitos x = if div x 10 == 0 then 1
else 1 + digitos (div x 10)



reduccion:: Int -> Int
reduccion x = if x < 10 then x
else  reduccion (mod x 10 + reduccion ( div x 10))

perm:: Int -> Int
perm x = if x == 1 then 1
else x * perm (x - 1)

vari:: Int -> Int -> Int
vari n m  = div (perm n) (perm (n-m))

combi:: Int -> Int -> Int
combi n m = div (perm n) ((perm (m) * perm (n-m)))


--Ejercicio 6

exercise6:: Bool->Bool->Bool->Bool
exercise6 a b c = ((a && (b || c)) || (!a && (c || b)))


--Ejercicio 7
--Escribir esto en consola y comparar con :t 
let f1 :: (a -> b -> a -> a) -> b -> b; f1 x y = y
let f2 :: (a -> b -> (a -> a)) -> (b -> b); f2 x y = y
let f3 :: a -> b -> a -> a -> b -> b; f3 u v w x y = y


 



