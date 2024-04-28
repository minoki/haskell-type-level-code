import Data.Proxy
import Data.Typeable

printType :: Typeable a => Proxy a -> IO ()
printType proxy = print (typeRep proxy)
