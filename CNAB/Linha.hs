module CNAB.Linha where

import qualified Data.List.Ordered as O
import CNAB.Bancos
import CNAB.Registros
import CNAB.Registros.Helpers

data Arquivo = Remessa | Retorno

newtype Linha = Linha {registros :: [Registro]}

instance Show Linha where
    show a = concatMap show $ registros a

emptyLinha :: Linha
emptyLinha = Linha []

appendRegistro :: Linha -> Registro -> Linha
appendRegistro (Linha []) a = Linha [a]
appendRegistro l          a = Linha $ O.insertSet a $ registros l

