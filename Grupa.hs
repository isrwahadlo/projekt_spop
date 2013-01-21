module Grupa where

type ID		=	Int
type Name	=	String

data Grupa = Grupa ID Name 

instance Show Grupa where
	show (Grupa gid nam) = "* Grupa \t numer \t\t" ++ show gid ++ "\n" ++
								"\t nazwa \t\t" ++ show nam ++ "\n";
								

grupaID :: Grupa -> ID
grupaID (Grupa x _) = x
								
grupaName :: Grupa -> Name
grupaName (Grupa _ name ) = name




