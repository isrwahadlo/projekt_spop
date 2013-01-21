module Generator where

wygenerujPlan = do {
                    {-zajecia <- wczytajPlik "zajecia"
                    przedmioty <- wczytajPlik "przedmioty"
                    sale <- wczytajPlik "sale"
                    grupy <- wczytajPlik "grupy"
                    wygenerujPlanZDanych zajecia przedmioty sale grupy  -}
}

wygenerujPlanZDanych zajecia przedmioty sale grupy = do {

}

-- sprawdza czy plan dotrzymuje ograniecznia na 2 przedmioty jednoczesnie
przedmiotyJednoczesnieOk [] zajecie = True
przedmiotyJednoczesnieOk z:zajecia zajecie = if (zajeciaID z /= zajeciaID zajecie && zajeciaStartTime z == zajeciaStartTime zajecie)
                                                then if (zajeciaSala z == zajeciaSala zajecie && zajeciaGrupa z == zajeciaGrupa zajecie)
                                                        then False
                                             else przedmiotyJednoczesnieOk zajecia zajecie

-- sprawdza czy plan dotrzymuje ograniczenia na jedno zajecie w czasie w sali
saleOk zajecia sala = zajeciaWSali zajecia sala 0
zajeciaWSali [] sala c = (c <= 1)
zajeciaWSali z:zajecia sala c = if (c > 1)
                                    then False
                                else
                                    if (zajeciaSala z == sala)
                                        then zajeciaWSali zajecia sala c+1
                                    else zajeciaWSali zajecia sala c

-- sprawdza czy plan dotrzymuje ograniczenia na liczbe godzin przedmiotu w tygodniu
przedmiotOk zajecia przedmiot grupa = sprawdzPrzedmiot zajecia przedmiot grupa 0
sprawdzPrzedmiot [] przedmiot grupa c = (przedmiotWeeklyLimit przedmiot == c)
sprawdzPrzedmiot z:zajecia przedmiot grupa c =  if przedmiotWeeklyLimit przedmiot > c
                                                    then False
                                                else if (zajeciaPrzedmiot z = przedmiot && zajeciaGrupa z == grupa)
                                                        then sprawdzPrzedmiot zajecia przedmiot grupa c+1
                                                     else sprawdzPrzedmiot zajecia przedmiot grupa c

-- sprawdza czy plan dotrzymuje ograniczenia na liczbe zajec w dniu dla grupy
grupaOk zajecia grupa = (sprawdzGrupe zajecia grupa 0 0 &&
                        sprawdzGrupe zajecia grupa 0 1 &&
                        sprawdzGrupe zajecia grupa 0 2 &&
                        sprawdzGrupe zajecia grupa 0 3 &&
                        sprawdzGrupe zajecia grupa 0 4 &&
                        sprawdzGrupe zajecia grupa 0 5)

sprawdzGrupe [] grupa c dzien = False
sprawdzGrupe z:zajecia grupa c dzien =  if c > 6
                                            then False
                                        else then
                                            if (zajeciaGrupa z = grupa && zajeciaDzien z == dzien)
                                                then sprawdzGrupe zajecia grupa c+1
                                            else sprawdzGrupe zajecia grupa c
