module Kern.Optimizer where

import Kern.AST

class Optimizable x where
    optimize :: x -> x

instance Optimizable Expr where
    optimize (AddExpr (PrimC a) (PrimC b)) = PrimC $ a + b
    optimize e = e
