module Optra.Operation 
    (
      Operation (..)
    , OperationSeq
    ) where


data Operation =
    Retain Int |
    Delete Int | 
    Insert String
    deriving (Eq, Show)

data OperationSeq = OperationSeq
  {
    baseLen :: Int
  , targetLen :: Int
  , operations :: [Operation]
  }


