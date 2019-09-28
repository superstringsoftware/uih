{-# LANGUAGE PostfixOperators #-}
module PreludeFixes where

-- 
import Data.StateVar (get)

infixl 9 .-
record.-field = field record

infix 8 ?=
(?=) sv = get sv

(•) = (.)