module DomainDriven.ServerOptions where

import           Prelude
import           Data.Char                      ( toLower )
import           DomainDriven.Server            ( ServerOptions(..) )



testServerOptions :: ServerOptions
testServerOptions = ServerOptions
    { renameConstructor = \case
                              "AddToCart" -> ["cart", "add"]
                              s           -> [fmap toLower s]
    , unitIsNoContent   = True
    }
