module Generator where
import Zajecia
import Przedmiot
import Sala
import Grupa


                    
wygenerujPlan = do 
                    listaPrzedmiotow <- wczytajPrzedmioty
                    listaSal <- wczytajSale
                    listaGrup <- wczytajGrupy
                    if wygenerujPlanZDanych listaPrzedmiotow listaSal listaGrup then do
                        putStrLn "Wygenerowano plan zajęć"
                    else do
                        putStrLn "Nie udało się wygenerować planu zajęć"


wygenerujPlanZDanych przedmioty sale grupy = sprawdzKombinacje (wygenerujKombinacje przedmioty sale grupy) przedmioty sale grupy

-- stworz liste wszystkich mozliwych planow zajec
wygenerujKombinacje :: [Przedmiot] -> [Sala] -> [Grupa] -> [[Zajecia]]
wygenerujKombinacje przedmioty sale grupy = dodajDoKombinacji przedmioty sale grupy

dodajDoKombinacji _ _ _ [] = []
dodajDoKombinacji przedmioty sale (g:grupy) = dodajDoKombinacji1 przedmioty sale g ++ dodajDoKombinacji przedmioty sale grupy

dodajDoKombinacji1 _ _ _ [] = []
dodajDoKombinacji1 przedmioty (s:sale) g = dodajDoKombinacji2 przedmioty s g ++ dodajDoKombinacji1 przedmioty sale g

dodajDoKombinacji2 _ _ _ [] = []
dodajDoKombinacji2 (p:przedmioty) s g = dodajDoKombinacji3 p s g ++ dodajDoKombinacji2 przedmioty s g

-- trzeba naprawic
dodajDoKombinacji3 _ _ _ _ = []




-- przeszukaj wszystkie warianty planu, zapisz plan i zwroc True jesli spelnia wymagania
sprawdzKombinacje :: [[Zajecia]] -> [Przedmiot] -> [Sala] -> [Grupa] -> Bool
sprawdzKombinacje [] _ _ _ = False
sprawdzKombinacje (z:kombinacje) przedmioty sale grupy = if sprawdzPlan z grupy przedmioty sale then do
                                                            zapiszZajecia z
                                                            return True
                                                        else 
                                                        	sprawdzKombinacje kombinacje przedmioty sale grupy

-- przeprowadz walidacje planu zajec
sprawdzPlan _ _ _ [] = True
sprawdzPlan zajecia grupy przedmioty (s:sale) = if sprawdzPlan1 zajecia grupy przedmioty s
                                                    then sprawdzPlan zajecia grupy przedmioty sale
                                                else False
sprawdzPlan1 _ _ [] _ = True
sprawdzPlan1 zajecia grupy (p:przedmioty) s = if sprawdzPlan2 zajecia grupy p s
                                                    then sprawdzPlan1 zajecia grupy przedmioty s
                                              else False

sprawdzPlan2 _ [] _ _ = True
sprawdzPlan2 zajecia (g:grupy) p s = if sprawdzPlan3 zajecia g p s
                                        then sprawdzPlan2 zajecia grupy p s
                                     else False

sprawdzPlan3 zajecia g p s = saleOk zajecia s && 
                             przedmiotOk zajecia p g &&
                             grupaOk zajecia g && 
                             przedmiotyJednoczesnie zajecia

-- sprawdza czy plan dotrzymuje ograniecznia na 2 przedmioty jednoczesnie
przedmiotyJednoczesnie zajecia = przedmiotyJednoczesnieOk1 zajecia zajecia

przedmiotyJednoczesnieOk1 zajecia [] = True
przedmiotyJednoczesnieOk1 zajecia (z:zaj) = if przedmiotyJednoczesnieOk2 zajecia z
                                                then przedmiotyJednoczesnieOk1 zajecia zaj
                                            else False

przedmiotyJednoczesnieOk2 [] zajecie = True
przedmiotyJednoczesnieOk2 (z:zajecia) zajecie =    	if zajeciaStartSlot z == zajeciaStartSlot zajecie
                                                        then if (zajeciaSalaNazwa z == zajeciaSalaNazwa zajecie && zajeciaGrupaNazwa z == zajeciaGrupaNazwa zajecie)
                                                            then False
                                                        else przedmiotyJednoczesnieOk2 zajecia zajecie
                                                    else przedmiotyJednoczesnieOk2 zajecia zajecie

-- sprawdza czy plan dotrzymuje ograniczenia na jedno zajecie w czasie w sali
saleOk zajecia sala = zajeciaWSali zajecia sala 0

zajeciaWSali :: [Zajecia] -> Sala -> Int -> Bool
zajeciaWSali [] sala c = (c <= 1)
zajeciaWSali (z:zajecia) sala c = if (c > 1)
                                    then False
                                else
                                    if (zajeciaSalaNazwa z == salaName sala)
                                        then zajeciaWSali zajecia sala (c+1)
                                    else zajeciaWSali zajecia sala c

-- sprawdza czy plan dotrzymuje ograniczenia na liczbe godzin przedmiotu w tygodniu
przedmiotOk zajecia przedmiot grupa = sprawdzPrzedmiot zajecia przedmiot grupa 0

sprawdzPrzedmiot :: [Zajecia] -> Przedmiot -> Grupa -> Int -> Bool
sprawdzPrzedmiot [] przedmiot grupa c = (przedmiotWeeklyLimit przedmiot == c)
sprawdzPrzedmiot (z:zajecia) przedmiot grupa c =  if przedmiotWeeklyLimit przedmiot > c
                                                    then False
                                                else if (zajeciaPrzedmiotNazwa z == przedmiotName przedmiot && zajeciaGrupaNazwa z == grupaName grupa)
                                                        then sprawdzPrzedmiot zajecia przedmiot grupa (c+1)
                                                     else sprawdzPrzedmiot zajecia przedmiot grupa c

-- sprawdza czy plan dotrzymuje ograniczenia na liczbe zajec w dniu dla grupy
grupaOk :: [Zajecia] -> Grupa -> Bool
grupaOk zajecia grupa = sprawdzGrupe zajecia grupa 1 0 &&
                        sprawdzGrupe zajecia grupa 2 0 &&
                        sprawdzGrupe zajecia grupa 3 0 &&
                        sprawdzGrupe zajecia grupa 4 0 &&
                        sprawdzGrupe zajecia grupa 5 0

sprawdzGrupe :: [Zajecia] -> Grupa -> Int -> Int -> Bool
sprawdzGrupe [] grupa dzien c = (c<=6)
sprawdzGrupe (z:zajecia) grupa dzien c =  if c > 6
                                            then False
                                        else if (zajeciaGrupaNazwa z == grupaName grupa && zajeciaDzien z == dzien)
                                                then sprawdzGrupe zajecia grupa dzien (c+1)
                                            else sprawdzGrupe zajecia grupa dzien c