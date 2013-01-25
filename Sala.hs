module Sala(salaName,dodajSale,zapiszSale,sprawdzIUtworzPlikSale,usunSale,listSale,iloscSale,pobierzSalaNazwa,wczytajSale,modyfikujSale) where
import IO
--import System.IO.Error
import Char
import TextUtil



type Name	=	String

salePlik = "sale.dat"

data Sala = Sala Name deriving (Show,Read,Eq)


salaName :: Sala -> Name
salaName (Sala name) = name




-- wczytaj sale z pliku i zwroc liste sal
wczytajSale = do
        hFile <- openFile salePlik ReadMode
        fileStr <- hGetContents hFile
        length fileStr `seq` hClose hFile
        hClose hFile
        return ((read fileStr) :: [Sala])
		
		
-- akcja do dodawania sal
dodajSale = do
        stareSale <- wczytajSale
        putStrLn "====================================="
        putStrLn "Dodawanie sal"
        putStrLn "Podaj nazwe sali: "
        nazwaSaliStr <- getLine
       
       -- putStr "Podaj nazwe sali: "
        do
                        let
                                -- stolikId = getNastStolikID stareStoliki 1
                               sala = Sala nazwaSaliStr
                        putStrLn "Podaj nazwe sali: "
                        if False then do
                            putStrLn "Podany numer sali juz istnieje."
                            else do
                            zapiszSale (stareSale ++ [sala])
                            putStrLn "Zapisano sale."
                        
                        
                
						

modyfikujSale = do
        putStrLn "====================================="
        putStrLn "Modyfikowanie sali"
        putStr "Podaj nazwe sali do modyfikacji: "
        nazwaSaliStr <- getLine
        stareSale <- wczytajSale
        do
                
                let sale = znajdzSale stareSale nazwaSaliStr
                if sale /= [] then do
                        --let sale = sale !! 0
                        --putStrLn "Znaleziono sale:"
                        putStrLn (sale2String sale)
                        
                        zapiszSale (usunSaleZListy stareSale nazwaSaliStr)
                        stareModSale <- wczytajSale
                        putStr "Podaj nowa nazwe sali: "
                        nowaNazwaSaliStr <- getLine
                        zapiszSale (stareModSale ++ [(Sala nowaNazwaSaliStr)])
                        putStrLn "Sale zmodyfikowano."
                        
                        else do
                        putStrLn "Nie znaleziono sali."


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

pobierzSalaNazwa :: [Sala] -> Int -> IO String
pobierzSalaNazwa (x:xs) num =
  do
    if num == 1 then return (salaName x)
    else pobierzSalaNazwa xs (num - 1)

	
-- zamien liste stolikow na napis, ktory mozna wypisac na ekranie
sale2String :: [Sala] -> String
sale2String [] = ""
sale2String (x:xs) = (sala2String x) ++ sale2String xs

-- zamien stolik na napis, ktory mozna wyisac na ekranie
sala2String  :: Sala -> String
sala2String (Sala nazwa) =
                "Sala : " ++ show nazwa++ "\n"


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
                hFile <- openFile salePlik ReadMode
                fileStr <- hGetContents hFile
                length fileStr `seq` hClose hFile
                hClose hFile
                return ()
                ) errorHandler
        where errorHandler e =
                if isDoesNotExistError e then do
                        putStrLn ("Tworzenie pliku: " ++ salePlik)
                        writeFile salePlik (show ([] :: [Sala]))
                        else
                        putStrLn ("Blad przy otwieraniu pliku: " ++ salePlik)

