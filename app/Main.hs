module Main where

import Control.Monad (filterM)
import Data.List (intercalate, sort)

-- | Hier kannst du vor dem Compilieren Omega anpassen.
omega = "abc"
empty = ""

-- | Ich verwende Chars als Elemente. Ein String ist eine Menge von Chars. Eine Liste
--   von Chars ist damit eine Familie.
--   Family ist damit der Datentyp eines Rings (bzw. ALgebra)
type Family = [String]

-- | Ein Kandidat für einen Ring (bzw. Algebra) ist eine Teilmenge der Potenzmenge.
--   Die Menge aller Teilmenge der Potenzmenge ist die Potenzmenge der Potenzmenge.
candidades = powerset $ powerset omega

-- | Dies sind die Ringe, die ich gefungen habe. Ich filtere alle Familien aus candidates 
--   heraus, die checkRing erfüllen.
rings    = filter checkRing candidades

-- | Analog zu rings nur eben Algebren
algebras = filter checkAlgebra candidades

-- | Gibt True, wenn es sich bei der übergebenen Familie um einen Ring handelt.
checkRing :: Family -> Bool
checkRing fam =    checkEmpty fam -- Ring-Axiom 1
                && checkMinus fam -- Ring-Axiom 2 
                && checkUnion fam -- Ring-Axiom 3

-- | Gibt True, wenn es sich bei der übergebenen Familie um eine Algebra handelt.
checkAlgebra :: Family -> Bool
checkAlgebra fam =    checkEmpty fam            -- Algebra-Axiom 1
                   && checkComplement omega fam -- Algebra-Axiom 2 
                   && checkUnion fam            -- Algebra-Axiom 3 

-- | Gibt True, wenn die leere Menge enthalten ist.
checkEmpty :: Family -> Bool
checkEmpty = elem empty

-- | Gibt True, wenn für je 2 Elemente a,b in der übergebenen Familie auch a \ b 
--   in der Familie ist.
checkMinus :: Family -> Bool
checkMinus fam = and $ do
    a <- fam
    b <- fam
    return $ elem (without a b) fam 

-- | Gibt True, wenn für je 2 Elemente a,b in der übergebenen Familie auch (a union b)
--   in der Familie ist.
checkUnion :: Family -> Bool
checkUnion fam = and $ do
    a <- fam
    b <- fam
    return $ elem (union a b) fam

--überprüft, ob zu jedem Element auch das Complement in der Familie enthalten ist.
checkComplement :: String -> Family -> Bool
checkComplement omega fam = and $ do
    a <- fam
    return $ elem (without omega a) fam

-- | Übergeben werden 2 geordnete Listen, die keine Duplikate enthalten. 
--   Zurückgegeben wird eine geordnete Liste ohne Duplikate, die alle Elemente
--   der Eingaben enthält. 
union :: Ord a => [a] -> [a] -> [a]
union [] ys = ys
union xs [] = xs
union (x:xs) (y:ys)
  | x == y = x : union xs ys
  | x <  y = x : union xs (y:ys)
  | x >  y = y : union (x:xs) ys

-- | Übergeben werden 2 geordnete Listen, die keine Duplikate enthalten. 
--   Zurückgegeben wird eine geordnete Liste ohne Duplikate, die alle Elemente
--   enthält, die in der ersten aber nicht in der zweiten Liste enthalten sind. 
without :: Ord a => [a] -> [a] -> [a]
without [] ys = []
without xs [] = xs
without (x:xs) (y:ys)
  | x == y =     without xs ys
  | x <  y = x : without xs (y:ys)
  | x >  y =     without (x:xs) ys

-- | Gibt die Potenzmenge zurück. Ziemlicher High-Haskell stuff das mit filterM zu schreiben.
powerset :: [a] -> [[a]]
powerset = filterM $ const [False,True]

-- | printFamily wandelt eine Familie so um, dass sie wie eine Menge aussieht. Zum Beispiel mache ich
--   geschweifte Klammern umzu. Mit dem Boolean
--   kann man angeben, ob man die leere Menge in Latex geschrieben wird. Das sieht dann auf mathefragen.de 
--   sehr hübsch aus aber in der Konsole würde ich besser False verwenden. 
printFamily :: Bool -> Family -> String
printFamily varnothing fam  = "{" ++ xs ++ "}"
  where
    xs                = intercalate "," $ map (printElem varnothing) fam
    printElem :: Bool -> String -> String
    printElem True "" = "\\( \\varnothing \\)"
    printElem _    x  = "{" ++ x ++ "}"

-- | printer druckt mehrzeilig die Familien untereinander.
printer :: Bool -> [Family] -> IO ()
printer varnothing = mapM_ (putStrLn . printFamily varnothing)


main :: IO ()
main = do
    putStrLn $ "Check for Omega = {" ++ omega ++ "}"
    putStrLn $ "Number of candidates = " ++ show (2 ^ (2 ^ length omega))
    putStrLn $ "These are the rings: "
    printer False rings
    putStrLn $ "\nThese are the algebras: "
    printer False algebras
