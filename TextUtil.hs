module TextUtil(sprawdzCzyLiczba,sprawdzLiczbeDlaOgraniczen,listaWyboru) where

import Char
import Maybe

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

listaWyboru listF lenF =
  do
    putStrLn "0. Powrot"
    listF
    wybor <- getLine
    let pnum = rUnsigned (mReadInt wybor)
    liczba <- lenF
    if pnum == -1 || pnum > liczba then
      do
        putStrLn "Niepoprawna wartosc."
        listaWyboru listF lenF
    else 
      return pnum
	  

mReadInt :: String -> Maybe Int
mReadInt = fmap fst.listToMaybe.reads

rUnsigned :: Maybe Int -> Int
rUnsigned Nothing = -1
rUnsigned (Just a)= a