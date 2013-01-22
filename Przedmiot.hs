module Przedmiot(przedmiotID, przedmiotName, przedmiotWeeklyLimit) where

type ID		=	Int
type Name	=	String
type WeeklyLimit =  Int


-- ID nie wiem czy wogole bedzie potrzebne - byc moze przy zapisach do pliku
-- Name wiadomo
-- WeeklyLimit - max. ilosc slotow czasowych dla przedmiotu w tygodniu
data Przedmiot = Przedmiot ID Name WeeklyLimit 


instance Show Przedmiot where
	show (Przedmiot id nam w_limit) = "* Przedmiot \t numer \t\t" ++ show id ++ "\n" ++
								"\t nazwa \t\t" ++ show nam ++ "\n" ++
								"\t limit \t\t" ++ show w_limit ++ "\n";
								

przedmiotID :: Przedmiot -> ID
przedmiotID (Przedmiot x _ _ ) = x
								
przedmiotName :: Przedmiot -> Name
przedmiotName (Przedmiot _ name _ ) = name

przedmiotWeeklyLimit :: Przedmiot -> WeeklyLimit
przedmiotWeeklyLimit (Przedmiot _ _ w_limit ) = w_limit

