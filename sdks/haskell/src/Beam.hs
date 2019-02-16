{-# LANGUAGE ExistentialQuantification, GADTs, InstanceSigs #-}

module Beam where

import Text.Printf

data KV k v = KV k v

data DoFn a b = DoFn {
    initialize :: IO ()
  , process    :: a -> IO [b]
  , flush      :: IO [b]
  , teardown   :: IO ()
}

-- noop DoFn, for overriding
doFn :: forall a b. DoFn a b
doFn = DoFn { initialize = return ()
            , flush = return []
            , teardown = return ()
            , process = (\x -> return []) }

data WindowFn a = WindowFn () -- hack

data Pipeline = Pipeline ()

data PCollection a where
  Impulse    :: PCollection ()
  ParDo      :: DoFn a b -> PCollection a -> PCollection b
  Window     :: WindowFn a -> PCollection a -> PCollection a
  Flatten    :: [PCollection a] -> PCollection a
  GroupByKey :: PCollection (KV k v) -> PCollection (KV k [v])

instance Functor PCollection where
  fmap :: (a -> b) -> PCollection a -> PCollection b
  fmap f = ParDo $ doFn {
    process = \x -> return [f x]
  }

flatMapElements :: (a -> [b]) -> PCollection a -> PCollection [b]
flatMapElements f = ParDo $ doFn {
  process = \x -> return $ f x
}

create :: [a] -> PCollection a
create xs = flatMapElements (\() -> xs) Impulse

readLines :: String -> PCollection String
readLines glob = create ["fake", "pcollection", "for", "you"]

-- in theory, returns files written or some such
writeLines :: PCollection String -> PCollection String
writeLines = ParDo $ doFn {
  process = putStrLn
}

