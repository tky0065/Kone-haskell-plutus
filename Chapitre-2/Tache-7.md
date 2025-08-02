## ✅ HC2T7 - Expressions booléennes

### Code :

```haskell
-- Main.hs

-- Expressions booléennes qui évaluent à True ou False
trueWithAnd :: Bool
trueWithAnd = True && True

falseWithOr :: Bool
falseWithOr = False || False

trueWithNot :: Bool
trueWithNot = not False

falseComparison :: Bool
falseComparison = 5 > 10

main :: IO ()
main = do
  putStrLn "=== Expressions booléennes ==="
  putStrLn ""
  
  -- True en utilisant &&
  putStrLn "1. True en utilisant && :"
  putStrLn ("True && True = " ++ show trueWithAnd)
  putStrLn ("(5 > 3) && (10 == 10) = " ++ show ((5 > 3) && (10 == 10)))
  putStrLn ""
  
  -- False en utilisant ||
  putStrLn "2. False en utilisant || :"
  putStrLn ("False || False = " ++ show falseWithOr)
  putStrLn ("(3 > 5) || (2 == 3) = " ++ show ((3 > 5) || (2 == 3)))
  putStrLn ""
  
  -- True en utilisant not
  putStrLn "3. True en utilisant not :"
  putStrLn ("not False = " ++ show trueWithNot)
  putStrLn ("not (5 < 3) = " ++ show (not (5 < 3)))
  putStrLn ""
  
  -- Une comparaison qui retourne False
  putStrLn "4. Comparaison qui retourne False :"
  putStrLn ("5 > 10 = " ++ show falseComparison)
  putStrLn ("\"hello\" == \"world\" = " ++ show ("hello" == "world"))
  putStrLn ("7 <= 5 = " ++ show (7 <= 5))
  putStrLn ""
  
  -- Exemples supplémentaires
  putStrLn "Exemples supplémentaires :"
  putStrLn ("True && not False = " ++ show (True && not False))
  putStrLn ("(10 /= 5) || (3 < 2) = " ++ show ((10 /= 5) || (3 < 2)))
```

### Explication :

* **True avec &&** :
  - `True && True = True` : Les deux opérandes sont vraies
  - `(5 > 3) && (10 == 10) = True` : Les deux comparaisons sont vraies

* **False avec ||** :
  - `False || False = False` : Les deux opérandes sont fausses
  - `(3 > 5) || (2 == 3) = False` : Les deux comparaisons sont fausses

* **True avec not** :
  - `not False = True` : Négation de False
  - `not (5 < 3) = True` : Négation d'une comparaison fausse

* **Comparaisons qui retournent False** :
  - `5 > 10 = False` : 5 n'est pas supérieur à 10
  - `"hello" == "world" = False` : Les chaînes ne sont pas égales
  - `7 <= 5 = False` : 7 n'est pas inférieur ou égal à 5

**Opérateurs de comparaison utiles** :
- `==` : égal à
- `/=` : différent de (not equal)
- `<`, `>`, `<=`, `>=` : comparaisons numériques
