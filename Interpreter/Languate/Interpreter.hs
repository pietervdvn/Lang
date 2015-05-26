module Languate.Interpreter where

{--

This module implements the expression interpreter

--}

{-Interprets a single clause. Gives back the typed expression-}
interpretClause	:: Clause -> TExpression
