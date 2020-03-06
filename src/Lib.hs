module Lib where

class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  -- [] a = [a]
  -- furry :: (a -> b) -> [a] -> [b]
  furry = map

-- Exercise 2
-- Relative Difficulty: 1
-- furry :: (a -> b) -> Maybe a -> Maybe b
instance Fluffy Maybe where
  furry f Nothing = Nothing
  furry f (Just a) = Just (f a)

-- Exercise 3
-- Relative Difficulty: 5
-- furry :: (a -> b) -> (t -> a) -> t -> b
instance Fluffy ((->) t) where
  furry f h t = f (h t)
  -- furry' = (.)

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)
s = EitherLeft (Left 3)
-- Exercise 4
-- Relative Difficulty: 5
-- furry :: (a -> b) -> (EitherLeft t) a -> (EitherLeft t) b
instance Fluffy (EitherLeft t) where
  furry f (EitherLeft(Left a)) = EitherLeft(Left(f a))
  furry f (EitherLeft(Right k)) = EitherLeft(Right k)

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry f (EitherRight(Left t)) = EitherRight(Left t)
  furry f (EitherRight(Right a)) = EitherRight((Right(f a)))

class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)

  furry' :: (a -> b) -> m a -> m b
  furry' f (ma) = banana (\x -> unicorn (f x)) ma

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  banana f [] = []
  banana f (x:xs) = f x ++ (banana f xs)
  unicorn = (:[])

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana f (Just a) = f a
  banana f Nothing = Nothing
  unicorn = Just

-- Exercise 9
-- Relative Difficulty: 6
-- (a -> t -> b) -> (t -> a) -> t -> b
instance Misty ((->) t) where
  banana f g t = f (g t) t
  unicorn a = \t -> a

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
  banana f (EitherLeft(Left a)) = f a
  banana f (EitherLeft(Right a)) = EitherLeft(Right a)
  unicorn = 

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  banana = error "todo"
  unicorn = error "todo"

-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean = error "todo"

-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple = error "todo"

-- Exercise 14
-- Relative Difficulty: 6
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy = error "todo"

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage = error "todo"

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 = error "todo"

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 = error "todo"

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 = error "todo"

newtype State s a = State {
  state :: (s -> (s, a))
}

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
  furry = error "todo"

-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  banana = error "todo"
  unicorn = error "todo"
