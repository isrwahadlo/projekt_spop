module Zajecia where

import IO
--import System.IO.Error
import Char
import TextUtil
import Przedmiot
import Grupa
import Sala

type PrzedmiotNazwa = String
type GrupaNazwa = String
type SalaNazwa = String
type Dzien = Int
type StartSlot = Int
type EndSlot = Int

zajeciaPlik="zajecia.dat"

data Zajecia = Zajecia PrzedmiotNazwa GrupaNazwa SalaNazwa Dzien StartSlot EndSlot deriving (Show,Read,Eq)

zajeciaPrzedmiotNazwa :: Zajecia -> PrzedmiotNazwa
zajeciaPrzedmiotNazwa (Zajecia p_nazwa _ _ _ _ _) = p_nazwa

zajeciaGrupaNazwa :: Zajecia -> GrupaNazwa
zajeciaGrupaNazwa (Zajecia _ g_nazwa _ _ _ _) = g_nazwa

zajeciaSalaNazwa :: Zajecia -> SalaNazwa
zajeciaSalaNazwa (Zajecia _ _ s_nazwa _ _ _) = s_nazwa

zajeciaDzien :: Zajecia -> Dzien
zajeciaDzien (Zajecia _ _ _ d _ _) = d


zajeciaStartSlot :: Zajecia -> StartSlot
zajeciaStartSlot (Zajecia _ _ _ _ ss _) = ss

zajeciaEndSlot :: Zajecia -> EndSlot
zajeciaEndSlot (Zajecia _ _ _ _ _ es) = es

instance Ord Zajecia where
  (Zajecia _ _ s_nazwa _ _ _)  `compare` (Zajecia _ _ s_nazwa2 _ _ _)  = s_nazwa `compare` s_nazwa2



-- wczytaj zajecia z pliku i zwroc liste zajec
wczytajZajecia = do
        hFile <- openFile zajeciaPlik ReadMode
        fileStr <- hGetContents hFile
        length fileStr `seq` hClose hFile
        hClose hFile
        return ((read fileStr) :: [Zajecia])

-- akcja do dodawania zajec
dodajZajecia returnF=  do 
    putStrLn "Wybierz przedmiot dla zajecia:"
    przedmiotNum <- listaWyboru listPrzedmioty iloscPrzedmioty
    stareZajecia <- wczytajZajecia
    if przedmiotNum == 0 then menuZajecia returnF
        else 
            do
            putStrLn "Wybierz grupe dla zajecia:"
            --gnum <- helperWybor listujGrupy liczGrupy
            grupaNum <- listaWyboru listGrupy iloscGrupy
            if grupaNum == 0 then menuZajecia returnF
               else
                   do
                      putStrLn "Wybierz sale dla zajecia:"
                      salaNum <- listaWyboru listSale iloscSale
                      if salaNum == 0 then menuZajecia returnF
                         else 
                             do
                                przedmiotyLista <- wczytajPrzedmioty
                                pnazwa <- pobierzPrzedmiotNazwa przedmiotyLista przedmiotNum
                                saleLista <- wczytajSale
                                snazwa <- pobierzSalaNazwa saleLista salaNum
                                grupyLista <- wczytajGrupy
                                gnazwa <- pobierzGrupaNazwa grupyLista grupaNum
                                (gdo, god, dzien) <- wczytajTermin pnazwa gnazwa snazwa
                                if (gdo == 0 && god == 0 && dzien == 0) then
                                  do
                                    putStrLn "Podany termin nie spelnia wymagan. Prosze sprobowac ponownie."
                                else
                                  do
                                    lista <- wczytajZajecia
                                    zapiszZajecia (lista ++ [(Zajecia pnazwa gnazwa snazwa gdo god dzien)])
                                    putStrLn "Pomyslnie dodano zajecie."

wczytajTermin p g s =
  do
    putStrLn "Godzina rozpoczęcia: (8 - 19)?"
    txt <- getLine
    let god = rUnsigned (mReadInt txt)
    if not (sprawdzStartSlot god) then
      do
        putStrLn "Niepoprawna!"
        wczytajTermin p g s
    else
      do
        putStrLn "Godzina zakończenia: (9 - 20)?"
        txt <- getLine
        let gdo = rUnsigned (mReadInt txt)
        if not (sprawdzEndSlot gdo god) then
          do
            putStrLn "Niepoprawna!"
            wczytajTermin p g s
        else
          do
            putStrLn "Podaj dzien zajecia"
            putStrLn "1. Poniedziałek"
            putStrLn "2. Wtorek"
            putStrLn "3. Środa"
            putStrLn "4. Czwartek"
            putStrLn "5. Piątek"
            txt <- getLine
            let dzien = rUnsigned (mReadInt txt)
            if not (sprawdzDzien dzien) then
              do
                putStrLn "Niepoprawny dzien!"
                wczytajTermin p g s
            else
              do
                przedmioty <- wczytajPrzedmioty
                grupy <- wczytajGrupy
                sale <- wczytajSale

                let lprzedmioty = znajdzPrzedmiot przedmioty p
                let lgrupy = znajdzGrupe grupy g
                let lsale = znajdzSale sale s
                lzajec <- wczytajZajecia
                if not (sprawdzTermin lzajec (head lprzedmioty) (head lgrupy) (head lsale) dzien god) then
                  do
                    putStrLn "Podany termin nie spelnia wymagan"
                    return (0,0,0)
                else
                  do
                    return (god, gdo, dzien)

	
--menu zajecia przeniesione z UI
menuZajecia returnF= do {
			putStrLn "(a) Wprowadzenie informacji o zajeciach";
			putStrLn "(d) Usuniecie informacji o zajeciu";
      putStrLn "(p) Pokaż zajęcia";
        			opt <- getLine;
							case opt of
                "a" -> do {
                  dodajZajecia returnF;
                  returnF;
                };
                "d" -> do {
                  usunZajecia;
                  returnF;
                };
                "p" -> do {
                  z <- wczytajZajecia;
                  pokazZajecia z 1;
                  returnF;
                };
                otherwise -> do {
                  putStrLn "Podano bledna wartosc";
                  returnF;
								};
					}
---


zapiszZajecia zajeciaLista = do
        writeFile zajeciaPlik (show zajeciaLista)

sprawdzIUtworzPlikZajec = do
        catch   (do
                putStrLn ("Sprawdzanie " ++ zajeciaPlik)
                hFile <- openFile zajeciaPlik ReadMode
                fileStr <- hGetContents hFile
                length fileStr `seq` hClose hFile
                hClose hFile
                return ()
                ) errorHandler
        where errorHandler e =
                if isDoesNotExistError e then do
                        putStrLn ("Tworzenie pliku: " ++ zajeciaPlik)
                        writeFile zajeciaPlik (show ([] :: [Zajecia]))
                        else
                        putStrLn ("Blad przy otwieraniu pliku: " ++ zajeciaPlik)


sprawdzDzien :: Int -> Bool
sprawdzDzien d | (d >= 1 && d <= 5) = True
                     | otherwise = False

-- sprawdzenie, czy godzina rozpoczecia zajec miesci sie miedzy 8 a 19.
sprawdzStartSlot :: Int -> Bool
sprawdzStartSlot god | (god >= 8 && god <= 19) = True
                     | otherwise = False

-- sprawdzenie, czy zajecia koncza sie miedzy 9 a 20
sprawdzEndSlot :: Int -> Int -> Bool
sprawdzEndSlot gdo god | (gdo >= 9 && gdo <= 20 && god < gdo) = True
                         | otherwise = False

-- Sprawdza czy sala nie jest juz zajeta w tym terminie
sprawdzSale :: [Zajecia] -> Sala -> Int -> Int -> Bool
sprawdzSale [] _ _ _ = True
sprawdzSale (z:xs) s d g =
  do
    if (zajeciaSalaNazwa z == salaName s) && (zajeciaDzien z == d) && (g >= zajeciaStartSlot z && g < zajeciaEndSlot z) then False
    else sprawdzSale xs s d g
    
-- Sprawdza czy grupa nie ma wiecej niz 6 godzin zajec dziennie
sprawdzGrupe :: [Zajecia] -> Grupa -> Int -> Int -> Int -> Bool
sprawdzGrupe [] _ _ _ sum | sum > 6 = False
                          | otherwise = True

sprawdzGrupe (z:xs) gr d g sum =
  do
    if (zajeciaGrupaNazwa z == grupaName gr) && (zajeciaDzien z == d) then sprawdzGrupe xs gr d g (sum + (zajeciaEndSlot z - zajeciaStartSlot z))
    else sprawdzGrupe xs gr d g sum
        
-- Sprawdza czy przedmiot nie odbywa sie w tym terminie
sprawdzPrzedmiot :: [Zajecia] -> Przedmiot -> Int -> Int -> Bool
sprawdzPrzedmiot [] _ _ _ = True
sprawdzPrzedmiot (z:xs) p d g =
  do
    if (zajeciaPrzedmiotNazwa z == przedmiotName p) && (zajeciaDzien z == d) && (g >= zajeciaStartSlot z && g < zajeciaEndSlot z) then False
    else sprawdzPrzedmiot xs p d g

-- Sprawdza czy przedmiot i grupa juz nie wystepuje
sprawdzPrzedmiotIGrupe :: [Zajecia] -> Przedmiot -> Grupa -> Bool
sprawdzPrzedmiotIGrupe [] _ _ = True
sprawdzPrzedmiotIGrupe (z:xs) p g =
  do
    if (zajeciaPrzedmiotNazwa z == przedmiotName p) && (zajeciaGrupaNazwa z == grupaName g) then False
    else sprawdzPrzedmiotIGrupe xs p g

sprawdzZajecia :: [Zajecia] -> Int -> Int -> Bool
sprawdzZajecia lista_zajec l_przedmiotow l_grup =
  do
    if ((length lista_zajec) >= (l_przedmiotow * l_grup)) then True
    else False

-- sprawdzanie, czy wprowadzony termin oraz grupa i sala nie koliduje z innymi zajeciami
sprawdzTermin :: [Zajecia] -> Przedmiot -> Grupa -> Sala -> Int -> Int -> Bool
sprawdzTermin lista_zajec przedmiot grupa sala d g =
  do
    if (sprawdzSale lista_zajec sala d g) && (sprawdzGrupe lista_zajec grupa d g 0) && (sprawdzPrzedmiot lista_zajec przedmiot d g) && (sprawdzPrzedmiotIGrupe lista_zajec przedmiot grupa) then True
    else False

pokazZajecia :: [Zajecia] -> Int -> IO ()
pokazZajecia [] _ = return ()
pokazZajecia (z:xs) num =
  do
    putStr ((show num) ++ ". Przedmiot: \"" ++ (show (zajeciaPrzedmiotNazwa z)) ++ "\", grupa: \"" ++ (show (zajeciaGrupaNazwa z)) ++ "\", sala: \"" ++ (show (zajeciaSalaNazwa z)) ++ "\"")
    putStr (", od: " ++ (show (zajeciaStartSlot z)) ++ ", do: " ++ (show (zajeciaEndSlot z)) ++ ", dzien: ")
    putStrLn ""
    pokazZajecia xs (num + 1)
    return ()

usunZajecia = do 
    writeFile zajeciaPlik (show ([] :: [Zajecia]))