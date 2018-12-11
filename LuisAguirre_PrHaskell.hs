--Luis Aguirre Galindo Doble Grado Informática Matemáticas

--Definicion de funciones
f1 = Si (No (V "p")) (Si (V "p") (Y (V "q") (No (V "q"))))
f2 = Y (V "p") (Si (No (V "q")) (No (V "p")))
f3 = Y (V "p") (Y (V "q") (O (No (V "q")) (V "r"))) 
f4 = No (Y (V "p") (O (V "p") (V "r")))
f5 = No (Y (O (V "r") (V "p")) (V "p")) --F4 y F5 son estructuralmente equivalentes
f6 = Y (V "p") (V "q")
f7 = Y (No (Y (V "p") (No (V "q")))) (V "p")

type Var = String
type Sust = [(String,Bool)] --Lista de pares (string, bool9.Lo utilizo para representar la asignacion de cada variable de una proposicion un valor de tipo bool.
data FProp = V Var | No FProp | Y FProp FProp | O FProp FProp | Si FProp FProp | Sii FProp FProp deriving Read

instance Eq FProp where
 (V x) == (V y) = x == y
 (No xs) == (No ys) = xs == ys
 (Y xs ys) == (Y ws zs) = (xs == ws && ys == zs) || (xs == zs && ys == ws)
 (O xs ys) == (O ws zs) = (xs == ws && ys == zs) || (xs == zs && ys == ws)
 (Si xs ys) == (Si ws zs) = (xs == ws && ys == zs)
 (Sii xs ys) == (Sii ws zs) = (xs == ws && ys == zs) || (xs == zs && ys == ws) --Aunque el enunciaba no lo indicaba, he considerado que en lo sii tampoco importa el orden 
 _ == _ = False
 
instance Ord FProp where --He redefinido los 4 operadores porque la igualdad de Eq es una igualdad puramente estructural, y no funciona correctamente con el orden establecido
 x <= y = consecuencia x y
 x >= y = consecuencia y x
 x < y = consecuencia x y && not (equivalente x y)
 x > y = consecuencia y x && not (equivalente y x)
 
instance Show FProp where
 show (V x) = id x
 show (No xs) = "~" ++ show xs
 show (Y xs ys) = "(" ++ show xs ++ " /\\ " ++ show ys ++ ")"
 show (O xs ys) = "(" ++ show xs ++ " \\/ " ++ show ys ++ ")"
 show (Si xs ys) = "(" ++ show xs ++ " -> " ++ show ys ++ ")"
 show (Sii xs ys) = "(" ++ show xs ++ " <-> " ++ show ys ++ ")"  

--Recordar que vars1 nos devuelve la lista de variables, pero con repeticiones
vars:: FProp -> [Var]  --Lista de variableres de una proposicion sin repeticiones 
vars f = map (head) (group (qsort(vars1 f)))  --Ordenamos las posibles repeticiones, las agrupamos y nos quedamos con una de cada una.

tautologia:: FProp -> Bool --Todas las posibles evaluaciones tienes que ser True
tautologia f = foldl (&&) True [evaluacion i f | i <- (permutacionesVar f)]

satisfactible:: FProp -> Bool --Alguna evaluacion tiene que ser True
satisfactible f = foldl (||) False [evaluacion i f | i <- (permutacionesVar f)]

consecuencia:: FProp -> FProp -> Bool --f2 es consecuencia de f1
consecuencia f1 f2 = tautologia (Si f1 f2)

equivalente:: FProp -> FProp -> Bool --f1 sii f2
equivalente f1 f2 = tautologia (Sii f1 f2)

consecuencias:: [FProp] -> [(FProp,[FProp])] --Empareja a cada elemento de una lista con todas los proposiciones de la lista que sean consecuencia
consecuencias xs = zip xs (consecuenciaPos xs) -- en particular, consigo misma

equivalentes:: [FProp] -> [[FProp]] --Cocientamos con la relación de ser equivalente
equivalentes [] = []  
equivalentes  xs = filter (equivalente (head xs)) xs : equivalentes (filter (not . (equivalente (head xs))) xs)


--Funciones auxiliares

--Vars1 devuelve todas las variables, con repeticiones
vars1:: FProp -> [Var]
vars1 (V x) = [x]
vars1 (No xs) = vars1 xs
vars1 (Y xs ys) = vars1 xs ++ vars1 ys
vars1 (O xs ys) = vars1 xs ++ vars1 ys
vars1 (Si xs ys) = vars1 xs ++ vars1 ys
vars1 (Sii xs ys) = vars1 xs ++ vars1 ys

qsort:: Ord a => [a] -> [a] --Ordenar una lista
qsort [] = [] 
qsort (x:xs) = qsort small ++ mid ++ qsort large
 where
  small = [y | y<-xs, y<x]
  mid   = [y | y<-xs, y==x] ++ [x]
  large = [y | y<-xs, y>x]
  
group:: Eq a => [a] -> [[a]] --Hace sublistas de elementos contiguos e iguales
group [] = []  
group xs = takeWhile (== head xs) xs : group (dropWhile (== head xs) xs)

evaluacion:: Sust -> FProp -> Bool  --Evaluamos una expresion, dada una sustitucion de las variables
evaluacion i (V x) = buscar x i
evaluacion i (No xs) = not (evaluacion i xs)
evaluacion i (Y xs ys) = evaluacion i xs && evaluacion i ys
evaluacion i (O xs ys) = evaluacion i xs || evaluacion i ys
evaluacion i (Si xs ys) = evaluacion i xs <= evaluacion i ys
evaluacion i (Sii xs ys) = evaluacion i xs == evaluacion i ys

buscar:: Var -> Sust -> Bool -- Buscar la variable x en la lista de sustituciones, y le asigna su valor booleano
buscar x i
 |x == fst (head i) = snd (head i)
 |otherwise = buscar x (tail i)
 
permutacionesBool :: Int -> [[Bool]]  --Todos los posibles valores booleanos que pueden tomar n variables
permutacionesBool 0     = [[]]
permutacionesBool n = map (False:) xs ++ map (True:) xs
 where xs = permutacionesBool (n-1)
 
 
permutacionesVar:: FProp -> [Sust] --Todos los posibles pares (variable, bool) de una formula
permutacionesVar f = map (zip xs) (permutacionesBool (length xs))
 where xs = vars f --Variables de una formula, sin repeticion
 
consecuenciaPos:: [FProp] -> [[FProp]] --Lista en la que en la posicion i, está la lista de consecuencias de la i-esima formula
consecuenciaPos xs = [ buscaConsecuencia i xs | i <- xs]

buscaConsecuencia:: FProp -> [FProp] -> [FProp] --Dada una formula buscamos todas las formulas que sean consecuencia de esta en una lista
buscaConsecuencia i xs = filter (consecuencia i) xs 



------------------------ENTRADA SALIDA---------------------------
main:: IO ()
main = do
  putStrLn"" 
  putStrLn""
  putStrLn "Elija una opción:"
  putStrLn "1.- Comprobar si una formula es tautologia"
  putStrLn "2.- Comprobar si una formula es satisfactible"
  putStrLn "3.- Comprobar si una formula es consecuencia de otra"
  putStrLn "4.- Comprobar si dos formulas son equivalentes"
  putStrLn "5.- Obtener las consecuencias de una lista de formulas"
  putStrLn "6.- Cocientar una lista de formulas mediante la relacion de ser equivalentes"
  putStrLn "0.- Salir"
  line <- getLine
  if (line == "1") then do {tautologiaIO; main;} else
   if (line == "2") then do {satisfactibleIO; main;} else
    if (line == "3") then do {consecuenciaIO; main;} else
     if (line == "4") then do {equivalenteIO; main;} else
      if (line == "5") then do {consecuenciasIO; main;} else
       if (line == "6") then do {equivalentesIO; main;} else
        if (line == "0") then putStrLn "Bye!" else do {putStrLn "Opcion no valida"; main;}
  
--Funciones auxiliares de entrada salida


tautologiaIO:: IO () --Pide al usuario que introduzca una formula (Ej: Y (V "p") (V "q")) y comprueba si es tautologia
tautologiaIO = do
 putStrLn "Introduzca una formula"
 f <- getFProp
 if (tautologia f) then putStrLn "Es tautologia!" else putStrLn "No es tautologia"
 
 
satisfactibleIO:: IO () --Pide al usuario que introduzca una formula y comprueba si es satisfactible
satisfactibleIO = do
 putStrLn "Introduzca una formula"
 f <- getFProp
 if (satisfactible f) then putStrLn "La formula es satisfactible!" else putStrLn "No es es satisfactible"
 
 
consecuenciaIO:: IO () --Pide al usuario que introduzca dos formulas y comprueba si la segunda es consecuencia de la primera
consecuenciaIO = do
 putStrLn "Introduzca la primera formula"
 f1 <- getFProp
 putStrLn "Introduzca la posible consecuencia"
 f2 <- getFProp
 if (consecuencia f1 f2) then putStrLn "La segunda formula es consecuencia logica de la primera!" 
 else putStrLn "La segunda formula no es consecuencia"
 

equivalenteIO:: IO () --Pide al usuario que introduzca dos formulas y comprueba si son equivalentes
equivalenteIO = do
 putStrLn "Introduzca la primera formula"
 f1 <- getFProp
 putStrLn "Introduzca la segunda formula"
 f2 <- getFProp
 if (equivalente f1 f2) then putStrLn "Las formulas son equivalentes!" 
 else putStrLn "Las formulas no son equivalentes"
 
consecuenciasIO:: IO () --Pide al usuario una lista de formulas y comprueba cuales son consecuencias de cada una
consecuenciasIO = do
 putStrLn "Introduzca la lista de formulas"
 f <- getFPropList
 print (consecuencias f)
 
equivalentesIO:: IO () --Pide al usuario que introduzca una lista de formulas y concienta dicha lista medienta la relacion
equivalentesIO = do  --de ser equivalentes
 putStrLn "Introduzca la lista de formulas"
 f <- getFPropList
 print (equivalentes f)


getFProp:: IO FProp  --Permite al usuario introducir una formula proposicional 
getFProp = do 
 line <- getLine
 return (read line::FProp)

getFPropList:: IO [FProp]  --Permite al usuario introducir una lista de formulas proposicionales 
getFPropList = do 
 line <- getLine
 return (read line::[FProp])