{-+
Command-Line Parsing Combinators
================================
-}
module Util.CmdLineParser(
  -- Parser type:
  P,
  -- Parser constructors:
  token,cmd,(!),(<@),(#@),chk,nil,oneof,many,arg,kw,opt,flag,readP,named,
  path,number,
  ( -: ),
  -- Parser destructors:
  --run,
  usage,parseAll
  ) where
import Text.PrettyPrint
import Control.Monad(MonadPlus(..),ap)
import Data.Maybe(isJust)
--import System(getArgs)
import Util.PM
import Util.Grammar

infixl 3 <@,`chk`,#@
infix 2 -:
infixr 1 !

{-+
The Parser Data Type
--------------------
-}
data P res = P Grammar (PM res)

instance Functor P where fmap f (P g p) = P g (fmap f p)

{-+
Parsing combinators
-------------------
-}

nil x = P Empty (return x)
P g1 f <@ P g2 a = P (Seq g1 g2) (f `ap` a)
named nt (P g p) = P (Nonterminal nt g) p
P g p -: descr = P (g :--- descr) p
token f s = P (Terminal s) (tokenP f s)
P g1 p1 ! P g2 p2 = P (Alt g1 g2) (p1 `mplus` p2)
many (P g p) = P (Many g) (manyP p)
opt (P g p) = P (Opt g) (fmap Just p `mplus` return Nothing)
oneof = foldr1 (!)

chk p p' = const `fmap` p <@ p'
cmd s p = nil p `chk` kw s

f #@ p = fmap f p

arg = token Just
kw s = token check s
  where check a = if a==s then Just () else Nothing

readP desc = token test desc
  where test arg = case reads arg of
		     (x,""):_ -> Just x
		     _ -> Nothing

flag s = isJust `fmap` opt (kw s)

{-+
Common tokens
-}
number:: (Read a,Num a) => P a
number = readP "<number>"
path = arg "<path>"

{-+
Extracting the documentation from a grammar
------------------------------------------
-}

usage prefix = render' . usageDoc prefix

usageDoc prefix (P g _) =
    (text "Usage:" $$
        nest 2 (nest 2 (text prefix<+>main) $$
                if null aux then empty else text "where" $$ vcat aux))
  where
    (main,aux) = ppGrammar g

{-+
Running a parser
----------------
-}
--run p = parseAll p =<< getArgs

parseAll (P _g p) = parsePM p
