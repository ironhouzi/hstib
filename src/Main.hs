module Main where

import Data.Char
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

data Letter
  = Ka | Kha | Ga | Nga
  | Ca | Cha | Ja | Nya
  | Ta | Tha | Da | Na
  | Pa | Pha | Ba | Ma
  | Tsa | Tsha | Dza | Wa
  | Zha | Za | Achung | Ya
  | Ra | La | Sha | Sa
  | Ha | A
  | I | U | E | O
  deriving (Eq, Ord, Read, Enum)

data SyllableComponent
  = Prefix
  | Superscribe
  | Subscribe
  | Root
  | Vowel
  | Suffix
  | SecondSuffix
  | Genitive
  deriving (Eq, Show, Ord, Enum)

data ParsedLetter =
  ParsedLetter (SyllableComponent, Letter)
  deriving (Eq, Show, Ord)

data ParsedSyllable = ParsedSyllable
  { components :: Set.Set SyllableComponent
  , parsedLetters :: Map.Map SyllableComponent Letter
  } deriving (Eq, Show)

emptyParsedSyllable :: ParsedSyllable
emptyParsedSyllable = ParsedSyllable Set.empty Map.empty

newParsedSyllable :: [(Letter, SyllableComponent)] -> ParsedSyllable
newParsedSyllable ls =
  foldr (\(l, s) -> addComponent l s) emptyParsedSyllable ls

addComponent :: Letter -> SyllableComponent -> ParsedSyllable -> ParsedSyllable
addComponent l s sc@(ParsedSyllable c pl)
  | not $ Set.member s c = ParsedSyllable (Set.insert s c) (Map.insert s l pl)
  | otherwise = sc

getComponent :: SyllableComponent -> ParsedSyllable -> Maybe Letter
getComponent sc (ParsedSyllable c pl) = Map.lookup sc pl

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

alphabet = map letterToString [(Ka) .. (O)]

consonants = Set.fromList [(Ka) .. (Ha)]

vowels = Set.fromList [(A) .. (O)]

prefixLetters = Set.fromList [Ga, Da, Ba, Ma, Achung]

superscribedLetters = Set.fromList [Ra, La, Sa]

subscribedLetters = Set.fromList [Ya, Ra, La, Wa]

suffixLetters = Set.fromList [Ga, Nga, Da, Na, Ba, Ma, Achung, Ra, La, Sa]

secondSuffixLetters = Set.fromList [Sa, Da]

ragoLetters = Set.fromList [Ka, Ga, Nga, Ja, Nya, Ta, Da, Na, Ba, Ma, Tsa, Dza]

lagoLetters = Set.fromList [Ka, Ga, Nga, Ca, Ja, Ta, Da, Pa, Ba, Ha]

sagoLetters = Set.fromList [Ka, Ga, Nga, Nya, Ta, Da, Pa, Ba, Ma, Tsa]

yataLetters = Set.fromList [Ka, Kha, Ga, Pa, Pha, Ba, Ma, Ha]

rataLetters =
  Set.fromList [Ka, Kha, Ga, Ta, Tha, Da, Na, Pa, Pha, Ba, Ma, Sa, Ha]

lataLetters = Set.fromList [Ka, Ga, Ba, Ra, Sa, Za]

wazurLetters =
  Set.fromList
    [Ka, Kha, Ga, Ca, Nya, Ta, Da, Tsa, Tsha, Zha, Za, Ra, La, Sha, Sa, Ha]

superstackRoots = ragoLetters `Set.intersection` lagoLetters
                              `Set.intersection` sagoLetters
                              `Set.intersection` yataLetters
                              `Set.intersection` rataLetters
                              `Set.intersection` lataLetters
                              `Set.intersection` wazurLetters

isPrefix :: Letter -> Bool
isPrefix l = Set.member l prefixLetters

isSuffix :: Letter -> Bool
isSuffix l = Set.member l suffixLetters

isSuffix2 :: Letter -> Bool
isSuffix2 l = Set.member l secondSuffixLetters

isVowel :: Letter -> Bool
isVowel l = A <= l && l <= O

isConsonant :: Letter -> Bool
isConsonant l = Ka <= l && l <= Ha

isSupersriber :: Letter -> Bool
isSupersriber l = Set.member l superscribedLetters

isRa :: Letter -> Bool
isRa l = l == Ra

isSa :: Letter -> Bool
isSa l = l == Sa

isLa :: Letter -> Bool
isLa l = l == La

isYa :: Letter -> Bool
isYa l = l == Ya

isWa :: Letter -> Bool
isWa l = l == Wa

isRagoLetter :: Letter -> Bool
isRagoLetter l = Set.member l ragoLetters

isSagoLetter :: Letter -> Bool
isSagoLetter l = Set.member l sagoLetters

isLagoLetter :: Letter -> Bool
isLagoLetter l = Set.member l lagoLetters

isYataLetter :: Letter -> Bool
isYataLetter l = Set.member l yataLetters
isRataLetter :: Letter -> Bool
isRataLetter l = Set.member l rataLetters
isLataLetter :: Letter -> Bool
isLataLetter l = Set.member l lataLetters
isWazurLetter :: Letter -> Bool
isWazurLetter l = Set.member l wazurLetters

superstackRoot :: Letter -> Bool
superstackRoot l = Set.member l superstackRoots

superscribes :: Letter -> Letter -> Bool
Ra `superscribes` l = isRagoLetter l
La `superscribes` l = isLagoLetter l
Sa `superscribes` l = isSagoLetter l
superscribes _ _ = False

subscribes :: Letter -> Letter -> Bool
Ya `subscribes` l = isYataLetter l
Ra `subscribes` l = isRataLetter l
La `subscribes` l = isLataLetter l
Wa `subscribes` l = isWazurLetter l
subscribes _ _ = False

data Ptree a =
  Pnode a
        [Ptree a]
  deriving (Eq, Show)

-- type Parser a = [Letter] -> [(a, [Letter])]
-- data Parser a = Parser ([Letter] -> [(a, [Letter])])

-- result :: a -> Parser a
-- result v = \inp -> [(v, inp)]

-- zero :: Parser a
-- zero = Parser (\inp -> [])

-- bind :: Parser a -> (a -> Parser b) -> Parser b
-- p `bind` f = \inp -> concat [f v inp' | (v, inp') <- p inp]

-- plus :: Parser a -> Parser a -> Parser a
-- p `plus` q = Parser (\inp -> (parse p inp ++ parse q inp))

newtype Parser a = Parser ([Letter] -> [(a, [Letter])])

parse :: Parser a -> [Letter] -> [(a, [Letter])]
parse (Parser f) s = f s

class ParseMonad m where
    result :: a -> m a
    bind :: m a -> (a -> m b) -> m b

instance ParseMonad Parser where
    -- result :: a -> Parser a
    result v = Parser (\inp -> [(v, inp)])
    -- bind :: Parser a -> (a -> Parser b) -> Parser b
    p `bind` f = Parser (\inp -> concat [parse (f v) inp' | (v, inp') <- parse p inp])

item :: Parser Letter
item = Parser (\inp -> case inp of
                 [] -> []
                 (x:xs) -> [(x, xs)])

myseq :: Parser a -> Parser b -> Parser (a, b)
p `myseq` q = p `bind` \x -> q `bind` \y -> result (x, y)

class ParseMonad m => ParseMonad0Plus m where
    zero :: m a
    plus :: m a -> m a -> m a

instance ParseMonad0Plus Parser where
    -- zero :: Parser a
    zero = Parser (\inp -> [])
    -- plus :: Parser a -> Parser a -> Parser a
    p `plus` q = Parser (\inp -> (parse p inp ++ parse q inp))

sat :: (Letter -> Bool) -> Parser Letter
sat p =
  item `bind` \x ->
    if p x
      then result x
      else zero

vowel :: Parser Letter
vowel = sat isVowel

rago :: Parser [Letter]
rago = sat isRa `bind` \x -> sat isRagoLetter `bind` \y -> result [x, y]

sago :: Parser [Letter]
sago = sat isSa `bind` \x -> sat isSagoLetter `bind` \y -> result [x, y]

lago :: Parser [Letter]
lago = sat isLa `bind` \x -> sat isLagoLetter `bind` \y -> result [x, y]

superscribe :: Parser [Letter]
superscribe = rago `plus` sago `plus` lago

rata :: Parser [Letter]
rata = sat isRataLetter `bind` \x -> sat isRa `bind` \y -> result [x, y]

yata :: Parser [Letter]
yata = sat isYataLetter `bind` \x -> sat isYa `bind` \y -> result [x, y]

lata :: Parser [Letter]
lata = sat isLataLetter `bind` \x -> sat isLa `bind` \y -> result [x, y]

wazur :: Parser [Letter]
wazur = sat isWazurLetter `bind` \x -> sat isWa `bind` \y -> result [x, y]

subscribe :: Parser [Letter]
subscribe = rata `plus` yata `plus` lata `plus` wazur

stack :: Parser [Letter]
stack = superscribe `plus` subscribe


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

main :: IO ()
main = do
  putStrLn (show Ka)
