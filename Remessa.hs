import qualified Data.ByteString as B

type TDocumento = Int
type CBanco = Int
type NBanco = String
type VArquivo = String
type TArquivo = Int
type TOperacao = Char

-- Helper
pad :: Show a => Int -> Char -> a -> String
pad 0 _ a     = show a
pad qtd chr a = (++) (replicate (qtd - length x) chr) x where x = show a

padZero :: Show a => Int -> a -> String
padZero qtd = pad qtd '0'

padSpace :: Show a => Int -> a -> String
padSpace qtd = pad qtd ' '

-- Arquivo
data Arquivo = Remessa | Retorno

getArquivo :: Arquivo -> TArquivo
getArquivo Remessa = 1
getArquivo Retorno = 2

getTipoOperacao :: Arquivo -> TOperacao
getTipoOperacao Remessa = 'R'
getTipoOperacao Retorno = 'T'

-- Documento
data Documento = CPF | CNPJ

getDocumento :: Documento -> TDocumento
getDocumento CPF  = 1
getDocumento CNPJ = 2

-- Banco
data Banco = Itau deriving(Show)

getBanco :: Banco -> CBanco
getBanco Itau = 341

getNomeBanco :: Banco -> NBanco
getNomeBanco Itau = "BANCO ITAU SA"

getVersaoArquivo :: Banco -> VArquivo
getVersaoArquivo Itau = "040"

-- Dados banco
data AgConta = AgConta { banco   :: Banco
                       , agencia :: Int
                       , conta   :: Int
                       , dac     :: Int
                       } deriving (Show)


-- Dados empresa
data Empresa = Empresa { nome      :: String
                       , inscricao :: Int
                       , ag_conta  :: AgConta
                       } deriving (Show)


data Arquivo240_Header = Arquivo240_Header { hr_banco  :: Banco
                                           , cod_lote  :: Int
                                           , tp_doc    :: Documento
                                           , empr      :: Empresa
                                           , data_ger  :: String
                                           , hora_ger  :: String
                                           , tp_arq    :: Arquivo
                                           }
getCodLote :: Arquivo240_Header -> String
getCodLote a = padZero 4 $ cod_lote a

instance Show Arquivo240_Header where
    show a = filter (/='"') $
       (show $ getBanco $ hr_banco a) ++
       getCodLote a ++
       "0" ++
       replicate 9 ' ' ++
       (show . getArquivo $ tp_arq a) ++
       (padZero 14 $ inscricao $ empr a) ++
       replicate 20 ' ' ++
       "0" ++
       (show . agencia . ag_conta $ empr a) ++
       " " ++
       replicate 7 '0' ++
       (show . conta . ag_conta $ empr a) ++
       " " ++
       (show . dac . ag_conta $ empr a) ++
       (padSpace 30 $ nome $ empr a) ++
       (padSpace 30 $ getNomeBanco $ hr_banco a) ++
       replicate 10 ' ' ++
       (show . getArquivo $ tp_arq a) ++
       data_ger a ++ hora_ger a ++
       (padZero 6 1) ++
       (getVersaoArquivo $ hr_banco a) ++
       replicate 5 '0' ++
       replicate 54 ' ' ++
       replicate 3 '0' ++
       replicate 12 ' '
