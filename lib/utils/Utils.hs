{-# LANGUAGE FlexibleContexts #-}
module Utils (
  module Utils,
  module Control.Applicative,
  module Control.Monad.State,
  (=~),
  isSpace,
  toLower,
  toUpper,
  intercalate,
  lift4,
  lift5,
  Bits(shift, complement, (.&.), (.|.), xor),
  foldl',
  deleteBy,
  splitOn,
  isJust,
  fromJust)
where

import Control.Applicative
import Text.Regex.Posix ( (=~) )
import Data.Char ( isSpace, toLower, toUpper )
import Data.List ( intercalate, foldl', deleteBy )
import Control.Applicative.HT (lift4, lift5)
import Control.Monad.State
import Data.Bits ( Bits(shift, complement, (.&.), (.|.), xor) )
import Data.List.Split ( splitOn )
import Data.Maybe( isJust, fromJust )


data Result a = Ok a | Err String | Fail

newtype Parser a b =
  Parser { runParser :: [a] -> Result (b, [a]) }

first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

instance Functor (Parser a) where
  fmap f (Parser p) = Parser $ fmap (first f) . p

instance Applicative (Parser a) where
  pure a = Parser (\s -> Ok (a, s))
  (Parser fp) <*> xp = Parser $ \s ->
    case fp s of
      Err e     -> Err e
      Fail      -> Fail
      Ok (f,s') -> runParser (f <$> xp) s'

instance Alternative (Parser a) where
  empty = Parser (const Fail)
  (Parser p1) <|> (Parser p2) = Parser (\s -> p1 s <|> p2 s)

instance Monad (Parser a) where
    return = pure

    Parser p1 >>= k = Parser q
      where
        q inp = case p1 inp of
          Err e -> Err e
          Fail  -> Fail
          Ok (x, rest) -> runParser (k x) rest

instance Show a => Show (Result a) where
  show (Ok s) = show s
  show (Err s) = s
  show Fail = "Fail"

instance Functor Result where
  fmap f (Ok x) = Ok (f x)
  fmap _ (Err e) = Err e
  fmap _ Fail = Fail

instance Applicative Result where
  pure = Ok
  (Ok f) <*> rX = f <$> rX
  (Err e) <*> _ = Err e
  Fail <*> _ = Fail

instance Alternative Result where
  empty = Fail
  Fail <|> r = r  -- will recover from Fail, but not Err
  l <|> _ = l

instance Monad Result where
  return = pure
  (Ok x) >>= f = f x
  (Err e) >>= _ = Err e
  Fail >>= _ = Fail


-- always fails, returns Err "error_msg"
errorParse :: String -> Parser a b
errorParse e = Parser $ const (Err e)

-- always fails, returns Fail
failParse :: Parser a b
failParse = Parser $ const Fail

-- succeeds if the next elements satisfies the condition p
satisfy :: Show a => (a -> Bool) -> Parser a a
satisfy p = Parser f
  where
    f [] = Fail
    f (x:xs)
        | p x       = Ok (x, xs)
        | otherwise = Fail

char :: (Show a, Eq a) => a -> Parser a a
char c = satisfy (==c)

-- succeeds if the next elements satisfies the condition p

expect :: (Show a, Eq a) => a -> Parser a a
expect c = Parser f
  where
    f [] = Fail
    f (x:xs)
        | x == c    = Ok (x, xs)
        | otherwise = Err $ "Expected " ++ show c ++ ", got " ++ show x

hasDuplicatesWith :: Eq a => [a] -> [a] -> Bool
hasDuplicatesWith _ [] = False
hasDuplicatesWith seen (x : xs) =
  (x `elem` seen) || hasDuplicatesWith (x : seen) xs

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates = hasDuplicatesWith []

showMaybe :: Show a => Maybe a -> String
showMaybe (Just x) = show x
showMaybe Nothing = ""

safeHead :: [a] -> Maybe a
safeHead (x : _) = Just x
safeHead [] = Nothing

append :: [a] -> a -> [a]
append xs x = xs ++ [x]

liftMaybe :: Monad m => (a -> StateT b m d) -> Maybe a -> StateT b m (Maybe d)
liftMaybe _ Nothing  = return Nothing
liftMaybe f (Just a) = fmap Just (f a)

getFst :: MonadState (a, b) m => m a
getFst = do
  gets fst

putFst :: MonadState (a, b) m => a -> m ()
putFst maps = do
  (_, n) <- get
  put (maps, n)

getSnd :: MonadState (a, b) m => m b
getSnd = do
  gets snd

putSnd :: MonadState (a, b) m => b -> m ()
putSnd n = do
  (val, _) <- get
  put (val, n)

makeUnique :: Monad m => String -> StateT (a, Int) m String
makeUnique name = do
  n <- getSnd
  putSnd (n + 1)
  return (name ++ "." ++ show n)

replace :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
replace x y maps =
  case lookup x maps of
    Just _ -> (x, y) : deleteBy (\(a, _) (a', _) -> a == a') (x, y) maps
    Nothing -> (x, y) : maps

writeResult :: String -> Result String -> IO ()
writeResult path (Ok s) = writeFile path s
writeResult _ (Err _) = return ()
writeResult _ Fail = return ()

-- to display errors
showErr :: Result a -> IO ()
showErr (Ok _) = return ()
showErr (Err s) = putStrLn s
showErr Fail = putStrLn "Fail"