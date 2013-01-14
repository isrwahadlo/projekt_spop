import Przedmiot
import System.Time
module Przedmiot where

type ID		=	Int
type Name	=	String
type StartTime =  CalendarTime
type EndTime =  CalendarTime


data Przedmiot = Przedmiot ID Name StarTime EndTime 


instance Show Przedmiot where
	show (Przedmiot id nam s_time e_time) = "* Przedmiot \t numer \t\t" ++ show id ++ "\n" ++
								"\t nazwa \t\t" ++ show nam ++ "\n" ++
								"\t start \t\t" ++ s_time ++ "\n" ++
								"\t koniec \t\t" ++ e_time ++ "\n"
								

PrzedmiotID :: Przedmiot -> ID
PrzedmiotID (Przedmiot x _) = x
								
PrzedmiotName :: Przedmiot -> Name
PrzedmiotName (Przedmiot _ name ) = name