module Main where

import qualified Data.Set as Set
import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)

import Alphabet


newtype Parser a = Parser ([Letter] -> [(a, [Letter])])

-- class Monad m where
--     result :: a -> m a
--     bind :: m a -> (a -> m b) -> m b

instance Monad Parser where
    -- return :: a -> Parser a
    return v = Parser (\inp -> [(v, inp)])
    -- bind :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = Parser (\inp -> concat [parse (f v) inp' | (v, inp') <- parse p inp])

parse :: Parser a -> [Letter] -> [(a, [Letter])]
parse (Parser f) s = f s

instance Functor Parser where
    -- liftM :: Monad m => (a1 -> r) -> m a1 -> m r
    -- fmap :: (a -> b) -> f a -> f b
    fmap f p = p >>= \x -> return (f x)

instance Applicative Parser where
    pure  = return
    -- ap :: Monad m => m (a -> b) -> m a -> m b
    (Parser f) <*> (Parser p) = return (f p)

item :: Parser Letter
item = Parser (\inp -> case inp of
                 [] -> []
                 (x:xs) -> [(x, xs)])

myseq :: Parser a -> Parser b -> Parser (a, b)
p `myseq` q = p >>= \x -> q >>= \y -> return (x, y)

class Monad m => ParseMonad0Plus m where
    zero :: m a
    plus :: m a -> m a -> m a

instance ParseMonad0Plus Parser where
    -- zero :: Parser a
    zero = Parser (\inp -> [])
    -- plus :: Parser a -> Parser a -> Parser a
    p `plus` q = Parser (\inp -> (parse p inp ++ parse q inp))

sat :: (Letter -> Bool) -> Parser Letter
sat p =
  item >>= \x ->
    if p x
      then return x
      else zero

vowel :: Parser Letter
vowel = sat isVowel

rago :: Parser [Letter]
rago = sat isRa >>= \x -> sat isRagoLetter >>= \y -> return [x, y]

sago :: Parser [Letter]
sago = sat isSa >>= \x -> sat isSagoLetter >>= \y -> return [x, y]

lago :: Parser [Letter]
lago = sat isLa >>= \x -> sat isLagoLetter >>= \y -> return [x, y]

superscribe :: Parser [Letter]
superscribe = rago `plus` sago `plus` lago

rata :: Parser [Letter]
rata = sat isRataLetter >>= \x -> sat isRa >>= \y -> return [x, y]

yata :: Parser [Letter]
yata = sat isYataLetter >>= \x -> sat isYa >>= \y -> return [x, y]

lata :: Parser [Letter]
lata = sat isLataLetter >>= \x -> sat isLa >>= \y -> return [x, y]

wazur :: Parser [Letter]
wazur = sat isWazurLetter >>= \x -> sat isWa >>= \y -> return [x, y]

subscribe :: Parser [Letter]
subscribe = rata `plus` yata `plus` lata `plus` wazur

stack :: Parser [Letter]
stack = superscribe `plus` subscribe

main :: IO ()
main = do
  putStrLn (show Ka)
