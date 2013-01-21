module Sala where

type ID		=	Int
type Name	=	String

data Sala = Sala ID Name 

instance Show Sala where
	show (Sala gid nam) = "* Sala \t numer \t\t" ++ show gid ++ "\n" ++
								"\t nazwa \t\t" ++ show nam ++ "\n" ++
								

SalaID :: Sala -> ID
SalaID (Sala x _) = x
								
SalaName :: Sala -> Name
SalaName (Sala _ name ) = name
