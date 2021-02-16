module DomainDriven.ServerOptions where

import           Prelude
import           Data.Char                      ( toLower )
import           DomainDriven.Server            ( ApiOptions(..) )



testServerOptions :: ApiOptions
testServerOptions = ApiOptions
    { renameConstructor = \case
                              "AddToCart" -> ["cart", "add"]
                              s           -> [fmap toLower s]
    , typenameSeparator = "_"
    }
