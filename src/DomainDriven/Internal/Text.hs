module DomainDriven.Internal.Text where

import           Prelude
import           Data.Text.Lens
import           Control.Lens                   ( taking
                                                , (%~)
                                                )
import           Data.Char                      ( toLower
                                                , toUpper
                                                )

lowerFirst :: IsText t => t -> t
lowerFirst = taking 1 text %~ toLower

upperFirst :: IsText t => t -> t
upperFirst = taking 1 text %~ toUpper


camelAppend :: (IsText t, Monoid t) => t -> t -> t
camelAppend a b = a <> upperFirst b
