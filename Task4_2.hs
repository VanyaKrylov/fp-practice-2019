module Task4_2 where

{-
  Задание 4.1
  Реализация монады над множеством из четырёх элементов.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

data FourOf a = FourOf {fi::a, sn::a, th::a, fo::a} deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12
instance Functor FourOf where
    fmap f (FourOf a b c d) = FourOf (f a) (f b) (f c) (f d)

instance Applicative FourOf where
    pure x = FourOf x x x x
    (<*>) (FourOf fa fb fc fd) m = FourOf (fi $ fmap fa m)
                                                           (sn $ fmap fb m) 
                                                           (th $ fmap fc m) 
                                                           (fo $ fmap fd m)  
    
{- FourOf 1 2 3 4 >>= \x ->
   FourOf 4 6 7 8 >>= \y ->
   return \z ->
   x+y
-}
instance Monad FourOf where
    (>>=) m k = FourOf{fi=fi $ k $ fi $ m, sn=sn $ k $ sn $ m,
                       th=th $ k $ th $ m, fo=fo $ k $ fo $ m}