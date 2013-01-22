module Sala(salaID, salaName) where

type ID		=	Int
type Name	=	String

data Sala = Sala ID Name 

instance Show Sala where
	show (Sala gid nam) = "* Sala \t numer \t\t" ++ show gid ++ "\n" ++
								"\t nazwa \t\t" ++ show nam ++ "\n";
								

salaID :: Sala -> ID
salaID (Sala x _) = x

								
salaName :: Sala -> Name
salaName (Sala _ name ) = name
