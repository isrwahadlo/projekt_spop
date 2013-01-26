module Main where
import IO
import Char
--import System.Time
--import System.Locale
--import System.Environment
import Sala
import Grupa
import Przedmiot
import Zajecia
import Generator

main = do {
		hSetBuffering stdin NoBuffering;
		putStrLn "------------------------------------------------";
		putStrLn "----------------------SPOP----------------------";
		putStrLn "Projekt: Program do ukladania planu zajec";
		putStrLn "-- Autorzy: Piotr Kalamucki, Filip Nabrdalik --";
		putStrLn "------------------------------------------------";
		
		sprawdzIUtworzPlikSale;
		sprawdzIUtworzPlikGrupy;
		sprawdzIUtworzPlikPrzedmioty;
		sprawdzIUtworzPlikZajec;
		menu;
}


----------------------------------------------------------------------------------------
--glowna petla programu
----------------------------------------------------------------------------------------
menu = do {
			putStrLn "-----------------MENU--------------------"; 
			putStrLn "(1) Edytuj sale";
			putStrLn "(2) Edytuj grupy";
			putStrLn "(3) Edytuj przedmioty";
			putStrLn "(4) Edytuj zajecia";
			putStrLn "(5) Generuj plan";
			{-
			putStrLn "(4) Edytuj zajecia";
			putStrLn "(5) Uloz plan zajec";
			putStrLn "(6) Wygeneruj plan zajec";
			putStrLn "(7) ??????";
			putStrLn "(8) Wyjscie";-}
			
			putStrLn "Wybierz polecenie:";
			cmd <- getLine;
 
			case cmd of
				"1" -> do {
							putStrLn "(a) Wprowadzenie informacji o sali";
							putStrLn "(d) Usuniecie informacji o sali";
							putStrLn "(m) Modyfikacja informacji o sali";
                            putStrLn "(v) Wyswietlenie informacji o salach";
							opt <- getLine;
							case opt of
								"a" -> do {
									
									dodajSale;
									menu;
								};
								"d" -> do {
									usunSale;
									menu;
								};
								"m" -> do {
									modyfikujSale;
									menu;
								};
								"v" -> do {
									wyswietlSale;
									menu;
								};
								
								otherwise -> do {
									putStrLn "Podano bledna wartosc";
									menu;
								};
						}
				"2" -> do {
							putStrLn "(a) Wprowadzenie informacji o grupie";
							putStrLn "(d) Usuniecie informacji o grupie";
							putStrLn "(m) Modyfikacja informacji o grupie";
							putStrLn "(v) Wyswietlenie informacji o grupach";
							opt <- getLine;
							case opt of
								"a" -> do {
									
									dodajGrupe;
									menu;
								};
								"d" -> do {
									usunGrupe;
									menu;
								};
								"m" -> do {
									modyfikujGrupe;
									menu;
								};
								"v" -> do {
									wyswietlGrupy;
									menu;
								};
								otherwise -> do {
									putStrLn "Podano bledna wartosc";
									menu;
								};
						}
				
				"3" -> do {
							putStrLn "(a) Wprowadzenie informacji o przedmiocie";
							putStrLn "(d) Usuniecie informacji o przedmiocie";
							putStrLn "(m) Modyfikacja informacji o przedmiocie";
							putStrLn "(v) Wyswietlenie informacji o przedmiotach";
							opt <- getLine;
							case opt of
								"a" -> do {
									
									dodajPrzedmiot;
									menu;
								};
								"d" -> do {
									usunPrzedmiot;
									menu;
								};
								"m" -> do {
									modyfikujPrzedmiot;
									menu;
								};
								"v" -> do {
									wyswietlPrzedmioty;
									menu;
								};
								otherwise -> do {
									putStrLn "Podano bledna wartosc";
									menu;
								};
						}
						
				"4" -> do {
							menuZajecia menu;
						}
				"5" -> do {
							putStrLn "Generuje plan";
							wygenerujPlan;
							menu
						}
				"7" -> return();
				otherwise -> do {
								putStrLn "Podano bledna wartosc";
								menu;
							};
		 }
