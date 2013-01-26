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
                                putStrLn "blabla:"
                                zapiszZajecia (stareZajecia++ [(Zajecia (pnazwa) (gnazwa) (snazwa) 2 4 5) ])
                                {-(gdo, god, dzien) <- wczytajTermin pnum gnum snum
                                if (gdo == 0 && god == 0 && dzien == 0) then
                                   do
                                   putStrLn "Podany termin nie spelnia wymagan. Prosze sprobowac ponownie."
                                   operacjeZajecia
                                     else
                                     do
                                   lista <- czytajZajecia
                                    objectToFile (lista ++ [(Zajecie (pnazwa) (gnazwa) (snazwa) gdo god dzien)]) "baza_zajecia"
                                    putStrLn "Pomyslnie dodano zajecie."
                                     operacjeZajecia
                                -}



	
--menu zajecia przeniesione z UI
menuZajecia returnF= do {
			putStrLn "(a) Wprowadzenie informacji o zajeciach";
			putStrLn "(d) Usuniecie informacji o zajeciu";
							opt <- getLine;
							case opt of
								"a" -> do {
									
									dodajZajecia returnF;
									returnF;
								};
								"d" -> do {
									--usunPrzedmiot;
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
