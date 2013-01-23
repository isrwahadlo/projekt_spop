module TextUtil(sprawdzCzyLiczba) where

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