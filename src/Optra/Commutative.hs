{-
 - Implementation of Commutative operations (compose & transform) for
 - Operational transform.
-}

module Optra.Commutative
    (
      compose
    , transform
    ) where

import qualified Data.List as DL
import qualified Data.Sequence as DS
import qualified Optra.Operation as OP 
    


-- | Merges operations o1 and o2 while preserving changes of both
--   Satisfies apply(apply(S, o1), o2) - apply(S, compose(o1, o2))
compose :: [Operation] -> [Operation] -> [Operation] -> Maybe [Operation]
compose [] [] res = DL.reverse res

compose (o:ops1) [] _ = Nothing 

compose [] (o:ops2) _ = Nothing

compose ((Delete n):ops1) ops2 res = compose ops1 ops2 ((Delete n):res)  

compose ops1 ((Insert s):ops2) res = compose ops1 ops2 ((Insert s):res)

compose ((Retain n):ops1) ((Retain m):ops2) res 
    | n < m     = compose ops1 ((Retain (m - n)):ops2) ((Retain n):res)
    | n > m     = compose ((Retain (n - m):ops1) ops2 ((Retain m):res)
    | otherwise = compose ops1 ops2 ((Retain n):res)

compose ((Insert s):ops1) ((Delete n):ops2) res
    | sLen < n  = compose ops1 ((Delete (n - sLen)):ops2) res
    | sLen > n  = compose (Insert (drop s n):ops1) ops2 res 
    | otherwise = compose ops1 ops2 res
  where
    | sLen = length s

compose ((Insert s):ops1) ((Retain n):ops2) res
    | sLen < n  = compose ops1 ((Retain (n - sLen)):ops2) ((Insert s):res)
    | sLen > n  = compose ops1 ((Retain (sLen - n)):ops2) ((Insert s):res)
    | otherwise = compose ops1 ops2 ((Insert s):res)
  where
    | sLen = length s

compose ((Retain n):ops1) ((Delete m):ops2) res 
    | n < m     = compose ops1 ((Delete (m - n)):ops2) ((Delete n):res)
    | n > m     = compose ((Retain (n - m):ops1) ops2 ((Delete m):res)
    | otherwise = compose ops1 ops2 ((Delete n):res)


-- | Transforms two operations o1 and o2 that happened concurrently and 
--   produces two operations o1' and o2' such that
--       apply(apply(S, o1), o2') = apply(apply(S, o2), o1')
transform :: [Operation] -> [Operation] -> Maybe ([Operation], [Operation])
transform [] [] res = (DL.reverse ops1', DL.reverse ops2')
  where
    (ops1, ops2') = res

transform (o:ops1) [] _ = Nothing

transform [] (o:ops2) _ = Nothing

transform ((Insert s):ops1) ops2 res = transform ops1 ops2 res'
  where
    (ops1', ops2') = res
    res'           = (((Insert s):ops1'), ((Retain (length s)):ops2'))

transform ops1 ((Insert s):ops2) res = transform ops1 ops2 res'
  where
    (ops1', ops2') = res
    res'           = (((Retain (length s)):ops2'), ((Insert s):ops1'))

transform ((Retain n):ops1) ((Retain m):ops2) res
    | n < m     = transform ops1 (retMN:ops2) ((retN:ops1'), (retN:ops2'))
    | n > m     = transform (retNM:ops1) ops2 ((retM:ops1'), (retM:ops2'))
    | otherwise = transform ops1 ops2 ((retN:ops1'), (retN:ops2'))
  where
    (ops1', ops2') = res
    (retN, retM)   = ((Retain n), (Retain m))
    (retNM, retMN) = ((Retain (n-m)), (Retain (m-n)))

transform ((Delete n):ops1) ((Delete m):ops2) res
    | n < m     = transform ops1 ((Retain (m - n)):ops2) res
    | n > m     = transform ((Retain (n - m)):ops1) ops2 res
    | otherwise = transform ops1 ops2 res 

transform ((Delete n):ops1) ((Retain m):ops2) res
    | n < m     = transform ops1 ((Retain (m - n):ops2) ((delN:ops1'), ops2') 
    | n > m     = transform ((Retain (n - m):ops1) ops2 ((delM:ops1'), ops2')
    | otherwise = transform ops1 ops2 ((delN:ops1'), ops2')
  where
    (ops1', ops2') = res
    (delN, delM)   = (Delete n, Delete m)

transform ((Retain n):ops1) ((Delete m):ops2) res
    | n < m     = transform ops1 ((Retain (m - n):ops2) (ops1', (delN:ops2')) 
    | n > m     = transform ((Retain (n - m):ops1) ops2 (ops1', (delM:ops2'))
    | otherwise = transform ops1 ops2 (ops1', (delN:ops2'))
  where
    (ops1', ops2') = res
    (delN, delM)   = (Delete n, Delete m)

