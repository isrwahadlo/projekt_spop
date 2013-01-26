module Przedmiot where
import IO
import Char
import TextUtil

type Name	=	String
type WeeklyLimit =  Int

przedmiotyPlik="przedmioty.dat"

--limit na ilosc godzin w tygodniu
minWeeklyLimit=0
maxWeeklyLimit=60


data Przedmiot = Przedmiot Name WeeklyLimit deriving (Show,Read,Eq)

								
przedmiotName :: Przedmiot -> Name
przedmiotName (Przedmiot name _ ) = name

przedmiotWeeklyLimit :: Przedmiot -> WeeklyLimit
przedmiotWeeklyLimit (Przedmiot _ w_limit ) = w_limit

-- wczytaj przedmioty z pliku i zwroc ich liste 
wczytajPrzedmioty = do
        hFile <- openFile przedmiotyPlik ReadMode
        fileStr <- hGetContents hFile
        length fileStr `seq` hClose hFile
        hClose hFile
        return ((read fileStr) :: [Przedmiot])
		
		
-- akcja do dodawania przedmiotow
dodajPrzedmiot = do
        putStrLn "====================================="
        putStrLn "Dodawanie przedmiotow"
        putStrLn "Podaj nazwe przedmiotu: "
        nazwaPrzedmiotuStr <- getLine
        putStrLn "Podaj tygodniowy limit godzin przedmiotu: "
        weeklyLimitStr <- getLine
        starePrzedmioty <- wczytajPrzedmioty
        if sprawdzCzyLiczba weeklyLimitStr == True then do
                        let
                                
                                weeklyLimit = read weeklyLimitStr :: Int
                                przedmiot = Przedmiot nazwaPrzedmiotuStr weeklyLimit
								-- sprawdzenie czy limit miesci sie w granicach tygodnia
                        if (sprawdzLiczbeDlaOgraniczen weeklyLimit minWeeklyLimit maxWeeklyLimit == False ) then do
                            putStrLn "Podano liczbe poza dopuszczalnym zakresem"
                            else do
                            
                                if (sprawdzCzyPrzedmiotIstnieje starePrzedmioty nazwaPrzedmiotuStr) then do
                                    putStrLn "Podany przedmiot juz istnieje."
                                    else do
                                        zapiszPrzedmioty (starePrzedmioty ++ [przedmiot])
                                        putStrLn "Zapisano przedmioty."
                else
                        putStrLn "Podano zla warosc." 
--modyfikacja przedmitow
modyfikujPrzedmiot = do
        putStrLn "====================================="
        putStrLn "Modyfikowanie przedmiotu"
        putStrLn "Podaj nazwe przedmiotu do modyfikacji: "
        nazwaPrzedmiotyStr <- getLine
        starePrzedmioty <- wczytajPrzedmioty
        do
                
                let przedmioty = znajdzPrzedmiot starePrzedmioty nazwaPrzedmiotyStr
                if przedmioty /= [] then do
                        
                        putStrLn (przedmioty2String przedmioty)
                        
                        zapiszPrzedmioty (usunPrzedmiotZListy starePrzedmioty nazwaPrzedmiotyStr)
                        stareModprzedmioty <- wczytajPrzedmioty
                        putStrLn "Podaj nowa nazwe przedmiotu: "
                        nowanazwaPrzedmiotyStr <- getLine
                        putStrLn "Podaj nowy limit czasu dla przedmiotu: "
                        limitPrzedmiotyStr <- getLine
                        if sprawdzCzyLiczba limitPrzedmiotyStr == True then do
                            let
                                weeklyLimit = read limitPrzedmiotyStr :: Int
                                przedmiot = Przedmiot nowanazwaPrzedmiotyStr weeklyLimit
                            zapiszPrzedmioty (stareModprzedmioty ++ [przedmiot])
                            putStrLn "Przedmiot zmodyfikowano."
                                    else do
                                           putStrLn "Niepoprawna wartosc."
                    else
                        putStrLn "Nie znaleziono przedmiotu."
--usuniecie przedmiotu						
usunPrzedmiot = do
        putStrLn "====================================="
        putStrLn "Usuwanie przedmiotu"
        starePrzedmioty <- wczytajPrzedmioty
        putStrLn "Przedmioty:"
        putStrLn (przedmioty2String starePrzedmioty)
        putStr "Podaj nazwe przedmiotu: "
        przedmiotNazwaStr <- getLine
        do
                let przedmiot = znajdzPrzedmiot starePrzedmioty przedmiotNazwaStr
                if przedmiot /= [] then do
                        
                        putStrLn "Znaleziono przedmiot:"
                        putStrLn (przedmioty2String przedmiot)
                        
                        zapiszPrzedmioty (usunPrzedmiotZListy starePrzedmioty przedmiotNazwaStr)
                        putStrLn "Przedmiot usunieto."
                   
                        else do
                        putStrLn "Nie znaleziono przedmiotu o podanej nazwie."

--wyswietlenie przedmiotow						
wyswietlPrzedmioty = do
                listaPrzed <- wczytajPrzedmioty
                putStrLn(przedmioty2String listaPrzed)	
				
-- zamien liste grup na napis, ktory mozna wypisac na ekranie
przedmioty2String :: [Przedmiot] -> String
przedmioty2String [] = ""
przedmioty2String (x:xs) = (przedmiot2String x) ++ przedmioty2String xs

-- zamien grupe na napis, ktory mozna wyisac na ekranie
przedmiot2String  :: Przedmiot -> String
przedmiot2String (Przedmiot nazwa limit) =
                "Przedmiot " ++ show nazwa ++ "; tygodniowy limit godzin: " ++ show limit ++ "\n"				

--funkcje odpowiedzialne za wyswietlenie listy przedmiotow podczas dodawania zajec				
pokazPrzedmiot :: [Przedmiot] -> Int -> IO ()
pokazPrzedmiot [] _ = return ()
pokazPrzedmiot (x:xs) num =
  do
    putStrLn ((show num) ++ "Przedmiot \"" ++ (show (przedmiotName x)) ++ "\", limit godzin " ++ (show (przedmiotWeeklyLimit x)))
    pokazPrzedmiot xs (num + 1)
    return ()

listPrzedmioty =
  do
    lista <- wczytajPrzedmioty
    pokazPrzedmiot lista 1
	
iloscPrzedmioty =
  do
    listaP <- wczytajPrzedmioty
    return (length listaP)

--funkcja pobierajaca przedmiot na podstawie indeksu i zwracajaca fun przedmiot
pobierzPrzedmiotNazwa :: [Przedmiot] -> Int -> IO String
pobierzPrzedmiotNazwa (x:xs) num =
  do
    if num == 1 then return (przedmiotName x)
    else pobierzPrzedmiotNazwa xs (num - 1)

--sprawdz czy przedmiot juz istnieje
sprawdzCzyPrzedmiotIstnieje :: [Przedmiot] -> String -> Bool
sprawdzCzyPrzedmiotIstnieje [] _ = False
sprawdzCzyPrzedmiotIstnieje (x:xs) nazwaPrzed =  przedmiotName (x) == nazwaPrzed || sprawdzCzyPrzedmiotIstnieje xs nazwaPrzed


-- pobierz grupe na podstawie podanego nr
znajdzPrzedmiot :: [Przedmiot] -> String -> [Przedmiot]
znajdzPrzedmiot [] _ = []
znajdzPrzedmiot (x:xs) id =
        if przedmiotName x == id then
        [x]
        else
        znajdzPrzedmiot xs id

-- usun grupe o podanym ID z listy
usunPrzedmiotZListy :: [Przedmiot] -> String -> [Przedmiot]
usunPrzedmiotZListy [] id = []
usunPrzedmiotZListy [przedmiot] id =
        if (przedmiotName przedmiot) == id then
                []
        else
                [przedmiot]
usunPrzedmiotZListy (s:reszta) id = (usunPrzedmiotZListy [s] id) ++ (usunPrzedmiotZListy reszta id)


-- zapisz przedmioty do pliku
zapiszPrzedmioty przedmiotyLista = do
        writeFile przedmiotyPlik (show przedmiotyLista)
 

-- sprawdza i tworzy nowe pliki jesli nie istnieja
sprawdzIUtworzPlikPrzedmioty = do
        catch   (do
                putStrLn ("Sprawdzanie " ++ przedmiotyPlik)
                hFile <- openFile przedmiotyPlik ReadMode
                fileStr <- hGetContents hFile
                length fileStr `seq` hClose hFile
                hClose hFile
                return ()
                ) errorHandler
        where errorHandler e =
                if isDoesNotExistError e then do
                        putStrLn ("Tworzenie pliku: " ++ przedmiotyPlik)
                        writeFile przedmiotyPlik (show ([] :: [Przedmiot]))
                        else
                        putStrLn ("Blad przy otwieraniu pliku: " ++ przedmiotyPlik)
--ilosc przedmiotow na liscie
liczPrzedmioty = do
    lista <- wczytajPrzedmioty
    return (length lista)						