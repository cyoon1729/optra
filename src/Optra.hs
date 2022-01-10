module Optra
    ( 
      Operation (..)
    , OperationSeq (..)
    , emptyOpSeq
    , addRetain
    , addDelete
    , addInsert
    , apply
    , inverse
    , transform
    , compose
    , OpInfo (..)
    , OpSeqInfo (..)
    , fromOpInfo
    , toOpInfo
    , fromOpSeqInfo
    , toOpSeqInfo
    , serializeOp
    , serializeOps
    , deserializeOp
    , deserializeOps
    ) where


import Optra.Operation
    (
      Operation (..)
    , OperationSeq (..)
    , emptyOpSeq
    , addRetain
    , addDelete
    , addInsert
    , apply
    , inverse
 
    )
import Optra.Commutative (transform, compose)
import Optra.Serialize 
    (
      OpInfo (..)
    , OpSeqInfo (..)
    , fromOpInfo
    , toOpInfo
    , fromOpSeqInfo
    , toOpSeqInfo
    , serializeOp
    , serializeOps
    , deserializeOp
    , deserializeOps
    )

