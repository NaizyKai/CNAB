module CNAB.Registros where

import CNAB.Bancos

data TipoRegistro = Numerico | Alfanumerico deriving(Eq, Show)

data Registro = Registro { posicao :: Int
                         , maxlen  :: Int
                         , tipo    :: TipoRegistro
                         , valor   :: [Char]
                         } deriving (Eq)


instance Ord Registro where
    a `compare` b = posicao a `compare` posicao b

instance Show Registro where
    show a = replicate s chr ++ x
        where x     = valor a
              qtd   = maxlen a
              s     = qtd - length x
              chr   = padValue a

padValue :: Registro -> Char
padValue a
    | tipo a == Numerico     = '0'
    | tipo a == Alfanumerico = ' '





