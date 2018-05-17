module Main where

import Data.Char
import Data.List
import qualified Data.Set as Set

data Letter
  = Ka
  | Kha
  | Ga
  | Nga
  | Ca
  | Cha
  | Ja
  | Nya
  | Ta
  | Tha
  | Da
  | Na
  | Pa
  | Pha
  | Ba
  | Ma
  | Tsa
  | Tsha
  | Dza
  | Wa
  | Zha
  | Za
  | Achung
  | Ya
  | Ra
  | La
  | Sha
  | Sa
  | Ha
  | A
  | I
  | U
  | E
  | O
  deriving (Eq, Ord, Read, Enum)

data SyllableComponent
  = Prefix
  | Superscribe
  | Subscribe
  | Root
  | Postfix
  | SecondPostfix
  | Genitive
  deriving (Eq, Show, Ord, Enum)

data ParsedLetter =
  ParsedLetter (Letter, SyllableComponent)
  deriving (Eq, Show)

letterToString :: Letter -> String
letterToString l =
  case l of
    Achung -> "'"
    A -> "a"
    I -> "i"
    U -> "u"
    E -> "e"
    O -> "o"
    xs -> init (map toLower (show xs))

instance Show Letter where
  show Ka = "ka"
  show Kha = "Kha"
  show Ga = "Ga"
  show Nga = "Nga"
  show Ca = "Ca"
  show Cha = "Cha"
  show Ja = "Ja"
  show Nya = "Nya"
  show Ta = "Ta"
  show Tha = "Tha"
  show Da = "Da"
  show Na = "Na"
  show Pa = "Pa"
  show Pha = "Pha"
  show Ba = "Ba"
  show Ma = "Ma"
  show Tsa = "Tsa"
  show Tsha = "Tsha"
  show Dza = "Dza"
  show Wa = "Wa"
  show Zha = "Zha"
  show Za = "Za"
  show Achung = "'"
  show Ya = "Ya"
  show Ra = "Ra"
  show La = "La"
  show Sha = "Sha"
  show Sa = "Sa"
  show Ha = "Ha"
  show A = "A"
  show I = "I"
  show U = "U"
  show E = "E"
  show O = "O"

stringToLetter :: String -> Letter
stringToLetter s =
  case s of
    "k" -> Ka
    "kh" -> Kha
    "g" -> Ga
    "g." -> Ga
    "ng" -> Nga
    "c" -> Ca
    "ch" -> Cha
    "j" -> Ja
    "ny" -> Nya
    "ta" -> Ta
    "th" -> Tha
    "d" -> Da
    "n" -> Na
    "p" -> Pa
    "ph" -> Pha
    "b" -> Ba
    "m" -> Ma
    "ts" -> Tsa
    "tsh" -> Tsha
    "dz" -> Dza
    "w" -> Wa
    "zh" -> Zha
    "z" -> Za
    "'" -> Achung
    "y" -> Ya
    "r" -> Ra
    "l" -> La
    "sh" -> Sha
    "s" -> Sa
    "h" -> Ha
    "a" -> A
    "i" -> I
    "u" -> U
    "e" -> E
    "o" -> O

-- letters = ['K', 'G', 'N', 'C', 'J', 'T', 'D', 'P', 'B', 'M', 'W', 'Z', '\'', 'Y', 'R', 'L', 'S', 'H', 'A', 'I', 'U', 'E', 'O']
alphabet = map letterToString [(Ka) .. (O)]

prefixLetters = Set.fromList [Ga, Da, Ba, Ma, Achung]

superscribedLetters = Set.fromList [Ra, La, Sa]

subscribedLetters = Set.fromList [Ya, Ra, La, Wa]

postfixLetters = Set.fromList [Ga, Nga, Da, Na, Ba, Ma, Achung, Ra, La, Sa]

secondPostfixLetters = Set.fromList [Sa, Da]

data Ptree a =
  Pnode a
        [Ptree a]
  deriving (Eq, Show)

ptAdd :: String -> Ptree Char
ptAdd (c:cs)
  | cs == [] = Pnode c []
  | otherwise = Pnode c [ptAdd cs]

ptInsert :: String -> [Ptree Char] -> [Ptree Char]
ptInsert [] ts = ts
ptInsert s [] = [ptAdd s]
ptInsert s@(c:cs) (x@(Pnode v ch):xs)
  | c == v = (Pnode v (ptInsert cs ch)) : xs
  | otherwise = x : (ptInsert s xs)

prefixTree = ptInsert "g." (foldr ptInsert [] alphabet)

ptNext :: String -> [Ptree Char] -> String -> Maybe String
ptNext [] _ _ = Nothing
ptNext _ [] _ = Nothing
ptNext s@(c:cs) (x@(Pnode v children):xs) acc
  | c /= v = ptNext s xs acc
  | cs == [] && c == v = Just (acc ++ [c])
  | c == v = ptNext cs children (acc ++ [c])
  | otherwise = Nothing

prefix :: String -> Maybe String
prefix s = ptNext s prefixTree ""

builder :: Char -> [String] -> [String]
builder c [] = [[c]]
builder c s@(x:xs)
  | prefix (c : x) == Nothing = ([c] : s)
  | otherwise = (c : x) : xs

build :: String -> [String]
build s = foldr builder [] s

letters :: String -> [Letter]
letters s = map stringToLetter (build s)

vowelIndexer :: [Letter] -> Int -> Maybe Int
vowelIndexer [] _ = Nothing
vowelIndexer (l:ls) n
  | elem l [(A) .. (O)] = Just n
  | otherwise = vowelIndexer ls (n + 1)

vowelIndex :: [Letter] -> Maybe Int
vowelIndex ls = vowelIndexer ls 0

main :: IO ()
main = do
  putStrLn (show Ka)
