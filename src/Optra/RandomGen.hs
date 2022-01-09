module Optra.RandomGen
    (
      genRandomString,
      genRandomOpSeq
    ) where


import qualified Test.RandomStrings as RS
import qualified Optra.Operation as OP


-- | Generate random ASCII string of input length.
genRandomString :: Int -> IO String
genRandomString n = RS.randomString RS.randomASCII n


-- | Generate random OperationSeq
genRandomOpSeq :: OP.OperationSeq
genRandomOpSeq = OP.emptyOpSeq
