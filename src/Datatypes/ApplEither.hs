{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module ApplEither where

-- ApplEither collects all errors
data ApplEither g b = Semigroup g => ALeft g | ARight b

deriving instance (Show g, Show b) => Show (ApplEither g b)


instance Functor (ApplEither g) where
  fmap f (ARight a) = ARight $ f a
  fmap f (ALeft e)  = ALeft e

instance Applicative (ApplEither g) where
  pure                  = ARight

  ARight f <*> ARight x = ARight $ f x
  ALeft e  <*> ARight _ = ALeft e
  ARight _ <*> ALeft e  = ALeft e
  ALeft e  <*> ALeft e' = ALeft $ e <> e'
