module Grupa(grupaID, grupaName,dodajGrupe,usunGrupe,sprawdzIUtworzPlikGrupy) where
import System.IO
import System.IO.Error
import Data.Char
import TextUtil

grupyPlik = "sale.dat"

type ID		=	Int
type Name	=	String

data Grupa = Grupa ID Name deriving (Show,Read,Eq)

								

grupaID :: Grupa -> ID
grupaID (Grupa x _) = x
								
grupaName :: Grupa -> Name
grupaName (Grupa _ name ) = name


-- wczytaj grupy z pliku i zwroc liste grup
wczytajGrupy = do
        hFile <- openFile grupyPlik ReadMode
        fileStr <- hGetContents hFile
        let grupy = (read fileStr) :: [Grupa]
        putStrLn ("Wczytano grup: " ++ (show (length grupy)))
        hClose hFile
        return grupy
		
		
-- akcja do dodawania grup
dodajGrupe = do
        putStrLn "====================================="
        putStrLn "Dodawanie grupy"
        putStr "Podaj ID grupy: "
        numerGrupyStr <- getLine
        putStr "Podaj nazwe grupy: "
        nazwaGrupyStr <- getLine
        stareGrupy <- wczytajGrupy
        if sprawdzCzyLiczba numerGrupyStr == True then do
                        let
                                
                                numerGrupy = read numerGrupyStr :: Int
                                grupa = Grupa numerGrupy nazwaGrupyStr
                        if (sprawdzCzyGrupaIstnieje stareGrupy numerGrupy) then do
                            putStrLn "Podany numer grupy juz istnieje."
                            else do
                            zapiszGrupy (stareGrupy ++ [grupa])
                            putStrLn "Zapisano sale."
                        
                        
                else
                        putStrLn "Podano zla liczbe."
						
-- usuniecie grupy
usunGrupe = do
        putStrLn "====================================="
        putStrLn "Usuwanie grupy"
        stareGrupy <- wczytajGrupy
        putStrLn "Grupy:"
        putStrLn (grupy2String stareGrupy)
        putStr "Podaj numer grupy: "
        grupaNumerStr <- getLine
        if sprawdzCzyLiczba grupaNumerStr then do
                let grupaNr = (read grupaNumerStr) :: Int
                let grupa = znajdzGrupe stareGrupy grupaNr
                if grupa /= [] then do
                        --let sale = sale !! 0
                        putStrLn "Znaleziono grupe:"
                        putStrLn (grupy2String grupa)
                        --putStrLn "Czy na pewno chcesz usunac ten stolik? [T/N]"
                        zapiszGrupy (usunGrupyZListy stareGrupy grupaNr)
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
                else do
                putStrLn "To nie jest liczba"
				
-- pobierz grupe na podstawie podanego nr
znajdzGrupe :: [Grupa] -> Int -> [Grupa]
znajdzGrupe [] _ = []
znajdzGrupe (x:xs) id =
        if grupaID x == id then
        [x]
        else
        znajdzGrupe xs id

-- usun grupe o podanym ID z listy
usunGrupyZListy :: [Grupa] -> Int -> [Grupa]
usunGrupyZListy [] id = []
usunGrupyZListy [grupa] id =
        if (grupaID grupa) == id then
                []
        else
                [grupa]
usunGrupyZListy (s:reszta) id = (usunGrupyZListy [s] id) ++ (usunGrupyZListy reszta id)

--sprawdz czy numer grupy juz istnieje
sprawdzCzyGrupaIstnieje :: [Grupa] -> Int -> Bool
sprawdzCzyGrupaIstnieje [] _ = False
sprawdzCzyGrupaIstnieje (x:xs) nrGrupy =  grupaID (x) == nrGrupy || sprawdzCzyGrupaIstnieje xs nrGrupy

-- zapisz sale do pliku
zapiszGrupy grupyLista = do
        writeFile grupyPlik (show grupyLista)
		
-- zamien liste grup na napis, ktory mozna wypisac na ekranie
grupy2String :: [Grupa] -> String
grupy2String [] = ""
grupy2String (x:xs) = (grupa2String x) ++ grupy2String xs

-- zamien grupe na napis, ktory mozna wyisac na ekranie
grupa2String  :: Grupa -> String
grupa2String (Grupa nr nazwa) =
                "Grupa nr. " ++ show nr++ "; nazwa: " ++ show nazwa ++ "\n"
	

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
