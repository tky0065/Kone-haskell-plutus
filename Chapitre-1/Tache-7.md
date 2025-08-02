
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
