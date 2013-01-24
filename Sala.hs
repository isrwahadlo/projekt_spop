module Sala(salaName,dodajSale,zapiszSale,sprawdzIUtworzPlikSale,usunSale,listSale,iloscSale) where
import System.IO
import System.IO.Error
import Data.Char
import TextUtil


type Name	=	String

salePlik = "sale.dat"

data Sala = Sala Name deriving (Show,Read,Eq)


salaName :: Sala -> Name
salaName (Sala name ) = name



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
        putStr "Podaj nazwe sali: "
        nazwaSaliStr <- getLine
        stareSale <- wczytajSale
        do
                        let
                                -- stolikId = getNastStolikID stareStoliki 1
                                --numerSali = read numerSaliStr :: Int
                                sala = Sala nazwaSaliStr
                        if (sprawdzCzySalaIstnieje stareSale nazwaSaliStr) then do
                            putStrLn "Podany numer sali juz istnieje."
                            else do
                            zapiszSale (stareSale ++ [sala])
                            putStrLn "Zapisano sale."
                        
                        
                
						
-- usuniecie sali
usunSale = do
        putStrLn "====================================="
        putStrLn "Usuwanie sali"
        stareSale <- wczytajSale
        putStrLn "Salei:"
        putStrLn (sale2String stareSale)
        putStr "Podaj nazwe sali: "
        salaNazwaStr <- getLine
        do
                
                let sale = znajdzSale stareSale salaNazwaStr
                if sale /= [] then do
                        --let sale = sale !! 0
                        putStrLn "Znaleziono sale:"
                        putStrLn (sale2String sale)
                       
                        zapiszSale (usunSaleZListy stareSale salaNazwaStr)
                        putStrLn "Sale usunieto."
                        
                        else do
                        putStrLn "Nie znaleziono sali o podanym ID."

						
						
--funkcje odpowiedzialne za wyswietlenie listy przedmiotow podczas dodawania zajec				
pokazSale :: [Sala] -> Int -> IO ()
pokazSale [] _ = return ()
pokazSale (x:xs) num =
  do
    putStrLn ( (show num) ++ " Sala " ++ (show (salaName x))  )
    pokazSale xs (num + 1)
    return ()

listSale =
  do
    lista <- wczytajSale
    pokazSale lista 1
	
iloscSale =
  do
    listaS <- wczytajSale
    return (length listaS)

-- zamien liste stolikow na napis, ktory mozna wypisac na ekranie
sale2String :: [Sala] -> String
sale2String [] = ""
sale2String (x:xs) = (sala2String x) ++ sale2String xs

-- zamien stolik na napis, ktory mozna wyisac na ekranie
sala2String  :: Sala -> String
sala2String (Sala nazwa) =
                "Sala nr. " ++ show nazwa++ "\n"


--sprawdz czy numer sali istnieje
sprawdzCzySalaIstnieje :: [Sala] -> String -> Bool
sprawdzCzySalaIstnieje [] _ = False
sprawdzCzySalaIstnieje (x:xs) nazwaSali =  salaName(x) == nazwaSali || sprawdzCzySalaIstnieje xs nazwaSali

-- pobierz sale na podstawie podanego nr
znajdzSale :: [Sala] -> String -> [Sala]
znajdzSale [] _ = []
znajdzSale (x:xs) id =
        if salaName x == id then
        [x]
        else
        znajdzSale xs id
		
-- usun stolik o podanym ID stolika z listy
usunSaleZListy :: [Sala] -> String -> [Sala]
usunSaleZListy [] id = []
usunSaleZListy [sale] id =
        if (salaName sale) == id then
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

