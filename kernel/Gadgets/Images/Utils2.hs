-- From InternetLib by Thomas Hallgren,
-- http://www.cs.chalmers.se/~hallgren/InternetLib/
module Utils2(module Utils2,chr,ord) where
import Data.Char
import Data.List(sort)

Nothing +++ x = x -- Haskell98: mplus, Haskell 1.4: ++
y +++ _ = y

-- For Haskell 1.3

-- Something is missing in Haskell 1.3...
--chr = toEnum :: (Int->Char)
--ord = fromEnum :: (Char->Int)

pair x y = (x,y)
mapPair (f,g) (x,y) = (f x,g y)
aboth f (x,y) = (f x,f y)
swap (x,y) = (y,x)
pairwith f x = (x,f x)
dup x = (x,x)
oo f g x y = f (g x y)

space n =
  if n<0
  then error "Utils2.space: negative argument"
  else replicate n ' '

--stripMaybeDef def Nothing = def
--stripMaybeDef _ (Just x) = x


words' cs =
      case break isSpace cs of
        ([],[]) -> []
	([],cs2) -> words'' cs2
	(cs1,cs2) -> cs1:words'' cs2

words'' cs =
      case span isSpace cs of
        ([],[]) -> []
	([],cs2) -> words' cs2
	(cs1,cs2) -> cs1:words' cs2


trim = reverse.trim1.reverse.trim1
  where trim1 = dropWhile isSpace

expandtabs n = exp n
  where
      exp k [] = []
      exp k ('\t':xs) = space k ++ exp n xs
      exp k (x:xs) = x:exp (if k==1 then n else k-1) xs

mix :: [[a]] -> [a] -> [a]
mix [] d = []
mix (x:xs) d = x++case xs of [] -> []; _ -> d ++ mix xs d

strToLower, strToUpper :: String -> String
strToLower = map toLower
strToUpper = map toUpper

addcr "" = ""
addcr ('\n':s) = '\r':'\n':addcr s
addcr (c:s) = c:addcr s

unmix sep xs =
  case break (sep==) xs of
    ([],[]) -> []
    (xs,[]) -> [xs]
    (xs,[sep]) -> [xs,[]]
    (xs,sep:ys) -> xs:unmix sep ys

bits p s n = n `div` (2^p) `mod` 2^s
bit p n = bits p 1 n /= 0

mynub xs = nubit.sort.nubit $ xs
  where
    nubit [] = []
    nubit (x:y:xs) | x == y = nubit (y:xs)
    nubit (x:xs) = x : nubit xs

isSpace' c = c/='\xa0' && isSpace c
