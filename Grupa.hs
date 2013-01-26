module Grupa where
import IO
--import System.IO.Error
import Char
import TextUtil
--import Control.Parallel.Strategies (rnf)

grupyPlik = "grupy.dat"


type Name	=	String

data Grupa = Grupa Name deriving (Show,Read,Eq)

								


								
grupaName :: Grupa -> Name
grupaName (Grupa name) = name



-- wczytaj grupy z pliku i zwroc liste grup
wczytajGrupy = do
        hFile <- openFile grupyPlik ReadMode
        fileStr <- hGetContents hFile 
        length fileStr `seq` hClose hFile
        hClose hFile
        return ((read fileStr) :: [Grupa])
		
		
-- akcja do dodawania grup
dodajGrupe = do
        stareGrupy <- wczytajGrupy
        putStrLn "==================Dodawanie grupy==================="
        putStrLn "Podaj nazwe grupy: "
        nazwaGrupyStr <- getLine
        
        do
                        let
                                
                                --numerGrupy = read numerGrupyStr :: Int
                                grupa = Grupa nazwaGrupyStr
                        if (sprawdzCzyGrupaIstnieje stareGrupy nazwaGrupyStr) then do
                            putStrLn "Podany numer grupy juz istnieje."
                            else do
                            zapiszGrupy (stareGrupy ++ [grupa])
                            putStrLn "Zapisano grupy."
                        
                        
modyfikujGrupe = do
        putStrLn "====================================="
        putStrLn "Modyfikowanie grup"
        putStrLn "Podaj nazwe grupy do modyfikacji: "
        nazwaGrupyStr <- getLine
        stareGrupy <- wczytajGrupy
        do
                
                let grupy = znajdzGrupe stareGrupy nazwaGrupyStr
                if grupy /= [] then do
                        --let grupy = grupy !! 0
                        --putStrLn "Znaleziono grupy:"
                        putStrLn (grupy2String grupy)
                        
                        zapiszGrupy (usunGrupyZListy stareGrupy nazwaGrupyStr)
                        stareModgrupy <- wczytajGrupy
                        putStrLn "Podaj nowa nazwe grupy: "
                        nowanazwaGrupyStr <- getLine
                        zapiszGrupy (stareModgrupy ++ [(Grupa nowanazwaGrupyStr)])
                        putStrLn "Grupe zmodyfikowano."
                        
                        else do
                        putStrLn "Nie znaleziono grupy."
						
-- usuniecie grupy
usunGrupe = do
        putStrLn "====================================="
        putStrLn "Usuwanie grupy"
        stareGrupy <- wczytajGrupy
        putStrLn "Grupy:"
        putStrLn (grupy2String stareGrupy)
        putStr "Podaj nazwe grupy: "
        grupaNazwaStr <- getLine
        do
                --let grupaNr = (read grupaNazwaStr) :: Int
                let grupa = znajdzGrupe stareGrupy grupaNazwaStr
                if grupa /= [] then do
                        --let grupy = grupy !! 0
                        putStrLn "Znaleziono grupe:"
                        putStrLn (grupy2String grupa)
                        --putStrLn "Czy na pewno chcesz usunac ten stolik? [T/N]"
                        zapiszGrupy (usunGrupyZListy stareGrupy grupaNazwaStr)
                        putStrLn "Grupe usunieto."
                        else do
                        putStrLn "Nie znaleziono grupy o podanym ID."

---
wyswietlGrupy = do
                 listaGrupy <- wczytajGrupy
                 putStrLn(grupy2String listaGrupy)	

--funkcje odpowiedzialne za wyswietlenie listy przedmiotow podczas dodawania zajec				
pokazGrupe :: [Grupa] -> Int -> IO ()
pokazGrupe [] _ = return ()
pokazGrupe (x:xs) num =
  do
    putStrLn ( (show num) ++ " Grupa \"" ++ (show (grupaName x))  )
    pokazGrupe xs (num + 1)
    return ()

listGrupy =
  do
    lista <- wczytajGrupy
    pokazGrupe lista 1
	
iloscGrupy =
  do
    listaP <- wczytajGrupy
    return (length listaP)
						

pobierzGrupaNazwa :: [Grupa] -> Int -> IO String
pobierzGrupaNazwa (x:xs) num =
  do
    if num == 1 then return (grupaName x)
    else pobierzGrupaNazwa xs (num - 1)

-- pobierz grupe na podstawie podanego nr
znajdzGrupe :: [Grupa] -> String -> [Grupa]
znajdzGrupe [] _ = []
znajdzGrupe (x:xs) id =
        if grupaName x == id then
        [x]
        else
        znajdzGrupe xs id

-- usun grupe o podanym ID z listy
usunGrupyZListy :: [Grupa] -> String -> [Grupa]
usunGrupyZListy [] id = []
usunGrupyZListy [grupa] id =
        if (grupaName grupa) == id then
                []
        else
                [grupa]
usunGrupyZListy (s:reszta) id = (usunGrupyZListy [s] id) ++ (usunGrupyZListy reszta id)

--sprawdz czy numer grupy juz istnieje
sprawdzCzyGrupaIstnieje :: [Grupa] -> String -> Bool
sprawdzCzyGrupaIstnieje [] _ = False
sprawdzCzyGrupaIstnieje (x:xs) nazwaGrupy =  grupaName (x) == nazwaGrupy || sprawdzCzyGrupaIstnieje xs nazwaGrupy

-- zapisz grupy do pliku
zapiszGrupy grupyLista = do
        writeFile grupyPlik (show grupyLista)
		
-- zamien liste grup na napis, ktory mozna wypisac na ekranie
grupy2String :: [Grupa] -> String
grupy2String [] = ""
grupy2String (x:xs) = (grupa2String x) ++ grupy2String xs

-- zamien grupe na napis, ktory mozna wyisac na ekranie
grupa2String  :: Grupa -> String
grupa2String (Grupa nazwa) =
                "Grupa nr. " ++ show nazwa++ "\n"
	

-- sprawdza i tworzy nowe pliki jesli nie istnieja
sprawdzIUtworzPlikGrupy = do
        catch   (do
                putStrLn ("Sprawdzanie " ++ grupyPlik)
                hFile <- openFile grupyPlik ReadMode
                fileStr <- hGetContents hFile
                length fileStr `seq` hClose hFile
                hClose hFile
                return ()
                ) errorHandler
        where errorHandler e =
                if isDoesNotExistError e then do
                        putStrLn ("Tworzenie pliku: " ++ grupyPlik)
                        writeFile grupyPlik (show ([] :: [Grupa]))
                        else
                        putStrLn ("Blad przy otwieraniu pliku: " ++ grupyPlik)
