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
