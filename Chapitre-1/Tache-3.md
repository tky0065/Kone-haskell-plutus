
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
