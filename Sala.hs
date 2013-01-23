module Sala(salaID, salaName,dodajSale,zapiszSale,sprawdzIUtworzPlikSale,usunSale) where
import System.IO
import System.IO.Error
import Data.Char
import TextUtil

type ID		=	Int
type Name	=	String

salePlik = "sale.dat"

data Sala = Sala ID Name deriving (Show,Read,Eq)


salaID :: Sala -> ID
salaID (Sala x _) = x

	
salaName :: Sala -> Name
salaName (Sala _ name ) = name



-- wczytaj sale z pliku i zwroc liste sal
wczytajSale = do
        hFile <- openFile salePlik ReadMode
        fileStr <- hGetContents hFile
        let sale = (read fileStr) :: [Sala]
        putStrLn ("Wczytano sal: " ++ (show (length sale)))
        hClose hFile
        return sale
		
		
-- akcja do dodawania sal
dodajSale = do
        putStrLn "====================================="
        putStrLn "Dodawanie sal"
        putStr "Podaj numer sali: "
        numerSaliStr <- getLine
        stareSale <- wczytajSale
        if sprawdzCzyLiczba numerSaliStr == True then do
                        let
                                -- stolikId = getNastStolikID stareStoliki 1
                                numerSali = read numerSaliStr :: Int
                                sala = Sala numerSali "aa"
                        if (sprawdzCzySalaIstnieje stareSale numerSali) then do
                            putStrLn "Podany numer sali juz istnieje."
                            else do
                            zapiszSale (stareSale ++ [sala])
                            putStrLn "Zapisano sale."
                        
                        
                else
                        putStrLn "Podano zla liczbe."
						
-- usuniecie sali
usunSale = do
        putStrLn "====================================="
        putStrLn "Usuwanie sali"
        stareSale <- wczytajSale
        putStrLn "Stoliki:"
        putStrLn (sale2String stareSale)
        putStr "Podaj numer sali: "
        salaNumerStr <- getLine
        if sprawdzCzyLiczba salaNumerStr then do
                let salaNr = (read salaNumerStr) :: Int
                let sale = znajdzSale stareSale salaNr
                if sale /= [] then do
                        --let sale = sale !! 0
                        putStrLn "Znaleziono sale:"
                        putStrLn (sale2String sale)
                        --putStrLn "Czy na pewno chcesz usunac ten stolik? [T/N]"
                        zapiszSale (usunSaleZListy stareSale salaNr)
                        putStrLn "Sale usunieto."
                        {-potwierdzenie <- getLine
                        case (map toLower potwierdzenie) of
                                "t" -> do
                                        zapiszStoliki (usunSaleZListy stareStoliki salaNr)
                                        putStrLn "Stolik usunieto."
                                _ -> do
                                        putStrLn "Anulowano"-}
                        else do
                        putStrLn "Nie znaleziono sali o podanym ID."
                else do
                putStrLn "To nie jest liczba"

-- zamien liste stolikow na napis, ktory mozna wypisac na ekranie
sale2String :: [Sala] -> String
sale2String [] = ""
sale2String (x:xs) = (sala2String x) ++ sale2String xs

-- zamien stolik na napis, ktory mozna wyisac na ekranie
sala2String  :: Sala -> String
sala2String (Sala nr nazwa) =
                "Sala nr. " ++ show nr++ "; nazwa: " ++ show nazwa ++ "\n"


--sprawdz czy numer sali istnieje
sprawdzCzySalaIstnieje :: [Sala] -> Int -> Bool
sprawdzCzySalaIstnieje [] _ = False
sprawdzCzySalaIstnieje (x:xs) nrSali =  salaID (x) == nrSali || sprawdzCzySalaIstnieje xs nrSali

-- pobierz sale na podstawie podanego nr
znajdzSale :: [Sala] -> Int -> [Sala]
znajdzSale [] _ = []
znajdzSale (x:xs) id =
        if salaID x == id then
        [x]
        else
        znajdzSale xs id
		
-- usun stolik o podanym ID stolika z listy
usunSaleZListy :: [Sala] -> Int -> [Sala]
usunSaleZListy [] id = []
usunSaleZListy [sale] id =
        if (salaID sale) == id then
                []
        else
                [sale]
usunSaleZListy (s:reszta) id = (usunSaleZListy [s] id) ++ (usunSaleZListy reszta id)


-- zapisz sale do pliku
zapiszSale sale = do
        writeFile salePlik (show sale)
	

-- sprawdza i tworzy nowe pliki jesli nie istnieja
sprawdzIUtworzPlikSale = do
        catch   (do
                putStrLn ("Sprawdzanie " ++ salePlik)
                plik <- readFile salePlik
                return ()
                ) errorHandler
        where errorHandler e =
                if isDoesNotExistError e then do
                        putStrLn ("Tworzenie pliku: " ++ salePlik)
                        writeFile salePlik (show ([] :: [Sala]))
                        else
                        putStrLn ("Blad przy otwieraniu pliku: " ++ salePlik)

