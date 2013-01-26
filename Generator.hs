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
