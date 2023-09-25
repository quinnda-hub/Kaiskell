module Evaluator where

newtype Evaluator a = Ev (Either String a)

instance Functor Evaluator where 
    fmap _ (Ev (Left e))  = Ev (Left e)
    fmap f (Ev (Right g)) = Ev (Right (f g)) 

instance Applicative Evaluator where 
    pure k = Ev (Right k)
    Ev (Left e)  <*> _ = Ev (Left e)
    Ev (Right f) <*> r = fmap f r

instance Monad Evaluator where 
    (Ev ev) >>= k = 
        case ev of 
            Left msg -> Ev (Left msg)
            Right v  -> k v
    return        = pure

instance MonadFail Evaluator where 
    fail msg = Ev (Left msg)
