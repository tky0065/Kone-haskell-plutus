## ✅ HC1T1 - Composition de Fonctions

### Code :

```haskell
-- Main.hs
double :: Int -> Int
double x = x * 2

increment :: Int -> Int
increment x = x + 1

doubleThenIncrement :: Int -> Int
doubleThenIncrement = increment . double

main :: IO ()
main = do
  let x = 5
  putStrLn ("Résultat de doubleThenIncrement " ++ show x ++ " : " ++ show (doubleThenIncrement x))
  putStrLn ("Vérification : double " ++ show x ++ " = " ++ show (double x))
  putStrLn ("Puis increment " ++ show (double x) ++ " = " ++ show (increment (double x)))
```

### Explication :

* `double x = x * 2` : prend un entier `x`, retourne le double.
* `increment x = x + 1` : prend un entier `x`, ajoute 1.
* `doubleThenIncrement = increment . double` :
  * `.` est **l'opérateur de composition** en Haskell.
  * Cela signifie : **double** d'abord, puis le résultat passe à **increment**.
  * Exemple : `doubleThenIncrement 5` calcule `increment (double 5)` soit `increment 10` = `11`.
* **Sortie** : `Résultat de doubleThenIncrement 5 : 11`

---

## ✅ HC1T2 - Fonction Pure : Aire d'un Cercle

### Code :

```haskell
-- Main.hs
circleArea :: Float -> Float
circleArea r = pi * r * r

main :: IO ()
main = do
  let radius1 = 2.0
  let radius2 = 5.0
  putStrLn ("Aire d'un cercle de rayon " ++ show radius1 ++ " : " ++ show (circleArea radius1))
  putStrLn ("Aire d'un cercle de rayon " ++ show radius2 ++ " : " ++ show (circleArea radius2))
  putStrLn ("Valeur de pi utilisée : " ++ show pi)
```

### Explication :

* Une **fonction pure** dépend uniquement de ses **paramètres** et produit toujours le même résultat.
* `circleArea` calcule **l'aire d'un cercle** avec la formule : A = π × r²
* `pi` est une constante **prédéfinie** en Haskell (≈ 3.14159).
* **Sortie** : `Aire d'un cercle de rayon 2.0 : 12.566371` et `Aire d'un cercle de rayon 5.0 : 78.53982`

---

## ✅ HC1T3 - Vérifier si un Nombre est Supérieur à 18

### Code :

```haskell
-- Main.hs
greaterThan18 :: Int -> Bool
greaterThan18 x = x > 18

main :: IO ()
main = do
  let ages = [15, 18, 20, 25, 12]
  putStrLn "Vérification des âges :"
  mapM_ (\age -> putStrLn (show age ++ " > 18 ? " ++ show (greaterThan18 age))) ages
  putStrLn ("Nombre de personnes majeures : " ++ show (length (filter greaterThan18 ages)))
```

### Explication :

* `greaterThan18` prend un **entier** `x` et renvoie **True** si `x > 18`.
* `mapM_` applique une action à chaque élément de la liste.
* `filter greaterThan18 ages` garde seulement les âges > 18.
* **Sortie** : Affiche pour chaque âge s'il est supérieur à 18, puis compte les majeurs.

---

## ✅ HC1T4 - Traitement de Données de Joueurs

### Code :

```haskell
-- Main.hs
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))

-- Données d'exemple
players :: [(String, Int)]
players = [("Alice", 30), ("Bob", 25), ("Carol", 40), ("David", 35), ("Eve", 20)]

-- Étape 1 : Extraire les noms
extractPlayers :: [(String, Int)] -> [String]
extractPlayers xs = [name | (name, _) <- xs]

-- Étape 2 : Trier par score décroissant
sortByScore :: [(String, Int)] -> [(String, Int)]
sortByScore = sortBy (comparing (Down . snd))

-- Étape 3 : Prendre les 3 meilleurs
topThree :: [(String, Int)] -> [(String, Int)]
topThree xs = take 3 xs

-- Étape 4 : Composition finale
getTopThreePlayers :: [(String, Int)] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore

main :: IO ()
main = do
  putStrLn "Liste originale des joueurs :"
  mapM_ (\(name, score) -> putStrLn ("  " ++ name ++ " : " ++ show score)) players
  
  putStrLn "\nJoueurs triés par score :"
  let sorted = sortByScore players
  mapM_ (\(name, score) -> putStrLn ("  " ++ name ++ " : " ++ show score)) sorted
  
  putStrLn "\nTop 3 des joueurs :"
  let top3 = topThree sorted
  mapM_ (\(name, score) -> putStrLn ("  " ++ name ++ " : " ++ show score)) top3
  
  putStrLn "\nNoms du top 3 :"
  let topNames = getTopThreePlayers players
  putStrLn ("  " ++ show topNames)
```

### Explication :

* **Étape 1** : `extractPlayers` utilise la **compréhension de liste** pour extraire les noms.
* **Étape 2** : `sortByScore` utilise `sortBy` avec `comparing (Down . snd)` pour trier par score décroissant.
* **Étape 3** : `topThree` utilise `take 3` pour garder les 3 premiers.
* **Étape 4** : `getTopThreePlayers` compose toutes les fonctions avec l'opérateur `.`
* **Sortie** : Affiche chaque étape du traitement des données.

---

## ✅ HC1T5 - Liste Infinie (Paresse)

### Code :

```haskell
-- Main.hs
infiniteNumbers :: [Int]
infiniteNumbers = [1..]

takeN :: Int -> [Int]
takeN n = take n infiniteNumbers

-- Autres exemples de listes infinies
infiniteOdds :: [Int]
infiniteOdds = [1,3..]

infiniteFibs :: [Int]
infiniteFibs = 0 : 1 : zipWith (+) infiniteFibs (tail infiniteFibs)

main :: IO ()
main = do
  putStrLn "Premiers 10 nombres naturels :"
  putStrLn ("  " ++ show (takeN 10))
  
  putStrLn "Premiers 8 nombres impairs :"
  putStrLn ("  " ++ show (take 8 infiniteOdds))
  
  putStrLn "Premiers 12 nombres de Fibonacci :"
  putStrLn ("  " ++ show (take 12 infiniteFibs))
  
  putStrLn "Le 100ème nombre naturel :"
  putStrLn ("  " ++ show (infiniteNumbers !! 99)) -- index 99 = 100ème élément
```

### Explication :

* `[1..]` crée une **liste infinie** grâce à la **paresse** (lazy evaluation).
* `take n` prend **seulement les n premiers éléments** sans calculer toute la liste.
* La **paresse** permet de définir des structures infinies sans problème de mémoire.
* `infiniteFibs` montre une définition récursive élégante de Fibonacci.
* **Sortie** : Différents exemples d'utilisation de listes infinies.

---

## ✅ HC1T6 - Signature de Type (Addition)

### Code :

```haskell
-- Main.hs
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

-- Version avec application partielle
addFive :: Int -> Int
addFive = addNumbers 5

-- Version plus générale
addGeneric :: Num a => a -> a -> a
addGeneric x y = x + y

main :: IO ()
main = do
  let a = 3
  let b = 4
  putStrLn ("Addition de " ++ show a ++ " et " ++ show b ++ " : " ++ show (addNumbers a b))
  
  putStrLn "Démonstration de l'application partielle :"
  putStrLn ("addFive 10 = " ++ show (addFive 10))
  putStrLn ("addFive 7 = " ++ show (addFive 7))
  
  putStrLn "Version générique avec des Float :"
  let x = 3.5 :: Float
  let y = 2.7 :: Float
  putStrLn ("Addition de " ++ show x ++ " et " ++ show y ++ " : " ++ show (addGeneric x y))
```

### Explication :

* Signature `Int -> Int -> Int` : deux entiers en entrée, retourne un entier.
* **Currification** : `addNumbers 5` crée une nouvelle fonction qui ajoute 5.
* **Application partielle** : très utile pour créer des fonctions spécialisées.
* `Num a =>` permet d'utiliser avec n'importe quel type numérique (Int, Float, Double, etc.).
* **Sortie** : Démontre l'addition normale, partielle et générique.

---

## ✅ HC1T7 - Conversion Fahrenheit → Celsius

### Code :

```haskell
-- Main.hs
fToC :: Float -> Float
fToC f = (f - 32) * (5/9)

cToF :: Float -> Float
cToF c = c * (9/5) + 32

-- Test avec des températures connues
testTemperatures :: [(Float, String)]
testTemperatures = [(32, "Point de congélation"), (212, "Point d'ébullition"), 
                   (98.6, "Température corporelle"), (0, "Zéro Fahrenheit")]

main :: IO ()
main = do
  putStrLn "Conversion Fahrenheit → Celsius :"
  mapM_ (\(temp, desc) -> 
    putStrLn ("  " ++ show temp ++ "°F (" ++ desc ++ ") = " ++ 
              show (fToC temp) ++ "°C")) testTemperatures
  
  putStrLn "\nConversion Celsius → Fahrenheit :"
  let celsiusTemps = [0, 25, 37, 100]
  mapM_ (\temp -> 
    putStrLn ("  " ++ show temp ++ "°C = " ++ show (cToF temp) ++ "°F")) celsiusTemps
  
  putStrLn "\nVérification (aller-retour) :"
  let testTemp = 25.0
  putStrLn ("  " ++ show testTemp ++ "°C → " ++ show (cToF testTemp) ++ 
            "°F → " ++ show (fToC (cToF testTemp)) ++ "°C")
```

### Explication :

* Formule : C = (F - 32) × (5/9)
* **Important** : parenthèses autour de `(5/9)` pour éviter la division entière.
* Ajout de la conversion inverse `cToF` pour les tests.
* Test avec des températures de référence connues.
* **Vérification** : conversion aller-retour pour valider les formules.
* **Sortie** : Conversions dans les deux sens avec des exemples concrets.

---

## ✅ HC1T8 - Fonction d'Ordre Supérieur

### Code :

```haskell
-- Main.hs
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Généralisation : appliquer n fois
applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN n f x = f (applyN (n-1) f x)

-- Fonctions de test
double :: Int -> Int
double x = x * 2

addOne :: Int -> Int
addOne x = x + 1

negateNum :: Int -> Int
negateNum x = -x

main :: IO ()
main = do
  let x = 3
  
  putStrLn "Fonction applyTwice avec différentes fonctions :"
  putStrLn ("  applyTwice double " ++ show x ++ " = " ++ show (applyTwice double x))
  putStrLn ("  applyTwice addOne " ++ show x ++ " = " ++ show (applyTwice addOne x))
  putStrLn ("  applyTwice negateNum " ++ show x ++ " = " ++ show (applyTwice negateNum x))
  
  putStrLn "\nFonction applyN (appliquer n fois) :"
  putStrLn ("  applyN 0 double " ++ show x ++ " = " ++ show (applyN 0 double x))
  putStrLn ("  applyN 3 double " ++ show x ++ " = " ++ show (applyN 3 double x))
  putStrLn ("  applyN 5 addOne " ++ show x ++ " = " ++ show (applyN 5 addOne x))
  
  putStrLn "\nAvec des listes :"
  let list = [1,2,3]
  putStrLn ("  applyTwice reverse " ++ show list ++ " = " ++ show (applyTwice reverse list))
  putStrLn ("  applyTwice tail " ++ show list ++ " = " ++ show (applyTwice tail list))
```

### Explication :

* **Fonction d'ordre supérieur** : prend une fonction comme paramètre.
* `(a -> a)` : fonction qui prend et retourne le même type.
* **Type polymorphe** `a` permet d'utiliser avec n'importe quel type.
* `applyN` généralise le concept pour appliquer une fonction n fois.
* **Exemples variés** : nombres, listes, différentes opérations.
* **Sortie** : Démontre la flexibilité des fonctions d'ordre supérieur.

