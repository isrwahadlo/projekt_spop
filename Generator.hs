module Generator where

import List

import Zajecia
import Przedmiot
import Sala
import Grupa

wygenerujPlan =
  do
    lprzedmioty <- wczytajPrzedmioty
    lgrupy <- wczytajGrupy
    lsale <- wczytajSale
    lzajecia <- wczytajZajecia
    planujZajecia lzajecia lprzedmioty lgrupy lsale 1 8

planujZajecia lista_zajec [] _ _ _ _ = 
  do
    let lk = sort lista_zajec
    liczba_grup <- liczGrupy
    liczba_przedm <- liczPrzedmioty
    if (sprawdzZajecia lk liczba_przedm liczba_grup) then
      do
        zapiszZajecia lk
        putStrLn "Udalo sie automatycznie ulozyc plan. Lista zajec zostala zapisana."
    else putStrLn "Nie udalo sie automatycznie ulozyc planu. Lista zajec pozostala bez zmian."

planujZajecia lista_zajec (p:lp) [] _ _ _ = 
  do
    lgrupy <- wczytajGrupy
    lsale <- wczytajSale
    planujZajecia lista_zajec lp lgrupy lsale 1 8

planujZajecia lista_zajec lp (g:lg) [] _ _ = 
  do
    lsale <- wczytajSale
    planujZajecia lista_zajec lp lg lsale 1 8

planujZajecia lista_zajec (p:lp) (gr:lg) (s:ls) d g =
  do
    if not (sprawdzDzien d) then planujZajecia lista_zajec ([p] ++ lp) ([gr] ++ lg) ls 1 8
    else if not ( (sprawdzStartSlot g) && (sprawdzEndSlot (g+(przedmiotWeeklyLimit p)) g) ) then planujZajecia lista_zajec ([p] ++ lp) ([gr] ++ lg) ([s] ++ ls) (d+1) 8
    else if not (sprawdzTermin lista_zajec p gr s d g) then planujZajecia lista_zajec ([p] ++ lp) ([gr] ++ lg) ([s] ++ ls) d (g+1)
    else
      do
        lsale <- wczytajSale
        planujZajecia (lista_zajec ++ [(Zajecia (przedmiotName p) (grupaName gr) (salaName s) d g (g+(przedmiotWeeklyLimit p)))]) ([p] ++ lp) lg lsale 1 8

-- -------------- Ukladanie planu --------------------------
-- sprawdzenie, czy dzien jest w zakresie roboczych dni tygodnia
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
--(Zajecia {przedmiotNazwa = pn, grupaNazwa = gn, salaNazwa = sn, startSlot = god, endSlot = gdo, dzien = dzie})

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