{-# LANGUAGE TemplateHaskell #-}

module DomainDriven.BeamProjection where

import Database.Beam.Postgres (Pg)
import Effectful
import Effectful.TH

data BeamProjection db :: Effect where
    RunPg :: Pg a -> BeamProjection db m a

type instance DispatchOf (BeamProjection db) = 'Dynamic

$(makeEffect ''BeamProjection)
