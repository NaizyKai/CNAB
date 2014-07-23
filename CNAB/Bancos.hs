module CNAB.Bancos where

type CBanco     = Int
type NBanco     = String
type VArquivo   = String

-- Banco
data Banco = Itau | Bradesco deriving(Show)

getBanco :: Banco -> CBanco
getBanco Itau     = 341
getBanco Bradesco = 237

getNomeBanco :: Banco -> NBanco
getNomeBanco Itau       = "BANCO ITAU SA"
getNomeBanco Bradesco   = ""

getVersaoArquivo :: Banco -> VArquivo
getVersaoArquivo Itau     = "040"
getVersaoArquivo Bradesco = ""
