module TextUtil(sprawdzCzyLiczba,sprawdzLiczbeDlaOgraniczen) where

import Data.Char


--	sprawdzenie czy to liczba ... przeniesienie do utils
sprawdzCzyLiczba :: String -> Bool
sprawdzCzyLiczba "" = False
sprawdzCzyLiczba [x] =
        if isDigit x == True then
        True
        else
        False
sprawdzCzyLiczba (x:xs) =
        if (isDigit x == True) then
        sprawdzCzyLiczba xs
        else
        False
		
--	sprawdzenie czy to liczba ... przeniesienie do utils
sprawdzLiczbeDlaOgraniczen :: Int -> Int -> Int -> Bool
sprawdzLiczbeDlaOgraniczen x a b = x >= a && x<=b

