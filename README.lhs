This package provides explicitly typed exceptions for Haskell
Example:

> import Control.Monad.Exception
> import Data.Typeable

> data Expr = Add Expr Expr | Div Expr Expr | Val Double
> eval (Val x)     = return x
> eval (Add a1 a2) = do
>    v1 <- eval a1
>    v2 <- eval a2
>    let sum = v1 + v2
>    if sum < v1 || sum < v2 then throw SumOverflow else return sum
> eval (Div a1 a2) = do
>    v1 <- eval a1
>    v2 <- eval a2
>    if v2 == 0 then throw DivideByZero else return (v1 / v2)

> data DivideByZero = DivideByZero deriving (Show, Typeable)
> data SumOverflow  = SumOverflow  deriving (Show, Typeable)

> instance Exception DivideByZero
> instance Exception SumOverflow

  GHCi infers the following types

 $> eval :: (Throws DivideByZero l, Throws SumOverflow l) => Expr -> EM l Double
 $> eval `catch` \ (e::DivideByZero) -> return (-1)  :: Throws SumOverflow l => Expr -> EM l Double

