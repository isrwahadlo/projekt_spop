module Main where
import IO
import Char
import System.Time
import System.Locale
import System.Environment

main = do {

		putStrLn "------------------------------------------------";
		putStrLn "----------------------SPOP----------------------";
		putStrLn "Projekt: Program do ukladania planu zajec";
		putStrLn "--Autorzy: Piotr Ka³amucki, Filip Nabrdalik --";
		putStrLn "------------------------------------------------";
		menu;
}


----------------------------------------------------------------------------------------
--glowna petla programu
----------------------------------------------------------------------------------------
menu = do {
			putStrLn "-----------------MENU--------------------"; 
			putStrLn "(1) Edytuj sale";
			putStrLn "(2) Edytuj przedmioty";
			putStrLn "(3) Edytuj zajecia";
			putStrLn "(4) Edytuj grupy";
			putStrLn "(5) Uloz plan zajec";
			putStrLn "(6) Wygeneruj plan zajec";
			putStrLn "(7) ??????";
			putStrLn "(8) Wyjscie";
			
			putStrLn "Wybierz polecenie:";
			cmd <- getLine;
 
			case cmd of
				"1" -> do {
							putStrLn "(a) Wprowadzenie informacji o sali";
							putStrLn "(d) Usuniecie informacji o sali";
							opt <- getLine;
							case opt of
								"a" -> do {
									putStrLn "Podaj numer sali: ";
									nr_sali <- getLine;
									--
									dodajSale nr_sali;
									menu;
								};
								"d" -> do {
									putStrLn "Podaj numer sali: ";
									nr_sali <- getLine;
									--
									usunSale nr_sali;
									menu;
								};
								otherwise -> do {
									putStrLn "Podano bledna wartosc";
									menu;
								};
						}
				"2" -> do {
							putStrLn "(a) Wprowadzenie informacji o przedmiocie";
							putStrLn "(m) Modyfikacja informacji o przedmiocie";
							putStrLn "(d) Usuniecie informacji o przedmiocie";
							opt <- getLine;
							case opt of
								"a" -> do {
									putStrLn "Podaj nazwe przedmiotu: ";
									nazwa_przedmiotu <- getLine;
									putStrLn "Podaj dzien [ 1-Poniedzialek; 5-Piatek ] : ";
									dzien_przedmiotu <- getLine;
									putStrLn "Podaj ilosc godzin: ";
									godziny_przedmiotu <- getLine;
									
									--
									dodajPrzedmiot nazwa_przedmiotu dzien_przedmiotu godziny_przedmiotu;
									menu;
								};
								"m" -> do {
									putStrLn "Podaj nazwe przedmiotu: ";
									nazwa_przedmiotu <- getLine;
									putStrLn "Podaj NOWA nazwe przedmiotu: ";
									nowa_nazwa_przedmiotu <- getLine;
									putStrLn "Podaj dzien [ 1-Poniedzialek; 5-Piatek ] : ";
									nowy_dzien_przedmiotu <- getLine;
									putStrLn "Podaj ilosc godzin: ";
									nowe_godziny_przedmiotu <- getLine;
									--
									modyfikujPrzedmiot nazwa_przedmiotu nowa_nazwa_przedmiotu nowy_dzien_przedmiotu nowe_godziny_przedmiotu;
									menu;
								};
								"d" -> do {
									putStrLn "Podaj nazwe przedmiotu: ";
									nazwa_przedmiotu <- getLine;
									--
									usunPrzedmiot nazwa_przedmiotu;
									menu;
								};
								otherwise -> do {
									putStrLn "Podano bledna wartosc";
									menu;
								};
						}
				"3" -> do {
							putStrLn "(a) Wprowadzenie informacji o zajeciach";
							putStrLn "(m) Modyfikacja informacji o zajeciach";
							putStrLn "(d) Usuniecie informacji o zajeciach";
							opt <- getLine;
							case opt of
								"a" -> do {
									putStrLn "Podaj nazwe przedmiotu z ktorego beda prowadzone zajecia: ";
									nazwa_przedmiotu <- getLine;
									putStrLn "Podaj grupe: ";
									grupa_zajec <- getLine;
									putStrLn "Podaj dzien zajec: ";
									dzien_zajec <- getLine;
									putStrLn "Podaj sale: ";
									sala_zajec <- getLine;
									--
									dodajPrzedmiot nazwa_przedmiotu dzien_przedmiotu godziny_przedmiotu;
									menu;
								};
								"m" -> do {
									putStrLn "Podaj nazwe przedmiotu: ";
									nazwa_przedmiotu <- getLine;
									putStrLn "Podaj NOWA nazwe przedmiotu: ";
									nowa_nazwa_przedmiotu <- getLine;
									putStrLn "Podaj dzien [ 1-Poniedzialek; 5-Piatek ] : ";
									nowy_dzien_przedmiotu <- getLine;
									putStrLn "Podaj ilosc godzin: ";
									nowe_godziny_przedmiotu <- getLine;
									--
									modyfikujPrzedmiot nazwa_przedmiotu nowa_nazwa_przedmiotu nowy_dzien_przedmiotu nowe_godziny_przedmiotu;
									menu;
								};
								"d" -> do {
									putStrLn "Podaj nazwe przedmiotu: ";
									nazwa_przedmiotu <- getLine;
									--
									usunPrzedmiot nazwa_przedmiotu;
									menu;
								};
								otherwise -> do {
									putStrLn "Podano bledna wartosc";
									menu;
								};
						}
				{-"4" -> do {
							putStrLn "Podaj kategorie wg ktorej chcesz szukac rezerwacji: ";
							putStrLn "(1)nr stolika (2)dzien 3)godzina (4)nazwisko (5)informacje dodatkowe: ";
							category <- getLine;
							putStrLn "Podaj ciag znakow jaki chcesz wyszukac: ";
							pattern <- getLine;
							case category of 
								"1" -> do{ wyszukajInfo "rezerwacje.txt" pattern 1 False; menu;}
								"2" -> do{ wyszukajInfo "rezerwacje.txt" pattern 2 False; menu;}
								"3" -> do{ wyszukajInfo "rezerwacje.txt" pattern 3 False; menu;}
								"4" -> do{ wyszukajInfo "rezerwacje.txt" pattern 4 False; menu;}
								"5" -> do{ wyszukajInfo "rezerwacje.txt" pattern 5 False; menu;}
								otherwise -> do {
									putStrLn "Podano bledna wartosc";
									menu;
								};
						}
				"5" -> do {
							wczytajPlik "stoliki.txt";
							menu
						}
				"6" -> do {
							wczytajPlik "rezerwacje.txt";
							menu
						}-}
				"7" -> return();
				otherwise -> do {
								putStrLn "Podano bledna wartosc";
								menu;
							};
		 }
