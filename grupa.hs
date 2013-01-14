
module Grupa where

type ID		=	Int
type Name	=	String

data Grupa = Grupa ID Name 

instance Show Grupa where
	show (Grupa gid nam) = "* Grupa \t numer \t\t" ++ show gid ++ "\n" ++
								"\t nazwa \t\t" ++ show nam ++ "\n" ++
								

GrupaID :: Grupa -> ID
GrupaID (Grupa x _) = x
								
GrupaName :: Grupa -> Name
GrupaName (Grupa _ name ) = name




