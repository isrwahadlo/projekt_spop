module Grupa(grupaName,dodajGrupe,usunGrupe,sprawdzIUtworzPlikGrupy,iloscGrupy,listGrupy) where
import System.IO
import System.IO.Error
import Data.Char
import TextUtil

grupyPlik = "grupy.dat"


type Name	=	String

data Grupa = Grupa Name deriving (Show,Read,Eq)

								


								
grupaName :: Grupa -> Name
grupaName (Grupa name) = name


-- wczytaj grupy z pliku i zwroc liste grup
wczytajGrupy = do
        hFile <- openFile grupyPlik ReadMode
        fileStr <- hGetContents hFile
        let grupy = (read fileStr) :: [Grupa]
        --putStrLn ("Wczytano grup: " ++ (show (length grupy)))
        hClose hFile
        return grupy
		
		
-- akcja do dodawania grup
dodajGrupe = do
        putStrLn "====================================="
        putStrLn "Dodawanie grupy"
        putStr "Podaj nazwe grupy: "
        nazwaGrupyStr <- getLine
        stareGrupy <- wczytajGrupy
        do
                        let
                                
                                --numerGrupy = read numerGrupyStr :: Int
                                grupa = Grupa nazwaGrupyStr
                        if (sprawdzCzyGrupaIstnieje stareGrupy nazwaGrupyStr) then do
                            putStrLn "Podany numer grupy juz istnieje."
                            else do
                            zapiszGrupy (stareGrupy ++ [grupa])
                            putStrLn "Zapisano grupy."
                        
                        

						
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
                        --let sale = sale !! 0
                        putStrLn "Znaleziono grupe:"
                        putStrLn (grupy2String grupa)
                        --putStrLn "Czy na pewno chcesz usunac ten stolik? [T/N]"
                        zapiszGrupy (usunGrupyZListy stareGrupy grupaNazwaStr)
                        putStrLn "Grupe usunieto."
                        {-potwierdzenie <- getLine
                        case (map toLower potwierdzenie) of
                                "t" -> do
                                        zapiszStoliki (usunGrupyZListy stareStoliki grupaNr)
                                        putStrLn "Stolik usunieto."
                                _ -> do
                                        putStrLn "Anulowano"-}
                        else do
                        putStrLn "Nie znaleziono grupy o podanym ID."

---

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

-- zapisz sale do pliku
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
                plik <- readFile grupyPlik
                return ()
                ) errorHandler
        where errorHandler e =
                if isDoesNotExistError e then do
                        putStrLn ("Tworzenie pliku: " ++ grupyPlik)
                        writeFile grupyPlik (show ([] :: [Grupa]))
                        else
                        putStrLn ("Blad przy otwieraniu pliku: " ++ grupyPlik)
