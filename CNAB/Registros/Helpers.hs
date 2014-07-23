module CNAB.Registros.Helpers where

import CNAB.Registros
import CNAB.Bancos


codBanco :: Banco -> Registro
codBanco a = Registro 1 3 Numerico $ show (getBanco a)
