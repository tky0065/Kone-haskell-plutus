## ✅ HC2T4 - Notation préfixe et infixe

### Code :

```haskell
-- Main.hs

main :: IO ()
main = do
  putStrLn "=== Notation préfixe et infixe ==="
  putStrLn ""
  
  -- Notation préfixe pour les opérateurs infixes
  putStrLn "Notation préfixe :"
  putStrLn ("(+) 5 3 = " ++ show ((+) 5 3))
  putStrLn ("(*) 10 4 = " ++ show ((*) 10 4))
  putStrLn ("(&&) True False = " ++ show ((&&) True False))
  putStrLn ""
  
  -- Notation infixe pour les fonctions préfixes
  putStrLn "Notation infixe :"
  putStrLn ("7 + 2 = " ++ show (7 + 2))
  putStrLn ("6 * 5 = " ++ show (6 * 5))
  putStrLn ("True && False = " ++ show (True && False))
  putStrLn ""
  
  -- Exemples avec des fonctions personnalisées
  putStrLn "Fonction personnalisée :"
  putStrLn ("multiply 3 4 = " ++ show (multiply 3 4))
  putStrLn ("3 `multiply` 4 = " ++ show (3 `multiply` 4))

-- Fonction personnalisée pour démonstration
multiply :: Int -> Int -> Int
multiply x y = x * y
```

### Explication :

* **Notation préfixe** : L'opérateur ou fonction vient AVANT ses arguments
  - `(+) 5 3` : Utilise `+` en notation préfixe avec des parenthèses
  - `(*) 10 4` : Utilise `*` en notation préfixe
  - `(&&) True False` : Utilise `&&` en notation préfixe

* **Notation infixe** : L'opérateur vient ENTRE ses arguments
  - `5 + 3` : Notation infixe normale
  - `10 * 4` : Notation infixe normale
  - `True && False` : Notation infixe normale

* **Conversion avec backticks** : 
  - `multiply 3 4` : Notation préfixe normale
  - `3 `multiply` 4` : Conversion en notation infixe avec des backticks

**Règle** : Utilisez des parenthèses `()` pour convertir infixe → préfixe, et des backticks `` ` `` pour convertir préfixe → infixe.
