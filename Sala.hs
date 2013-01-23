module Sala(salaID, salaName,dodajSale,zapiszSale,sprawdzIUtworzPlikSale) where
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
        if sprawdzCzyLiczba numerSaliStr == True
                then do
                        let
                                -- stolikId = getNastStolikID stareStoliki 1
                                numerSali = read numerSaliStr :: Int
                                sala = Sala numerSali "aa"
                        zapiszSale (stareSale ++ [sala])
                        putStrLn "Zapisano stoliki."
                else
                        putStrLn "Podano zla liczbe."

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

