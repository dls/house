-- From InternetLib by Thomas Hallgren,
-- http://www.cs.chalmers.se/~hallgren/InternetLib/
module ParsOps2 where
--import Utils2
import Data.Array.Unboxed

infixl 3 `ap`, `chk`, `bind`
infixl 2 `orelse`

-- "Parser t a" is a parser that parses a sequence of "t":s into an "a".
newtype Parser t a = P (Buf t->Either String (a,Buf t))
  -- input->(error_msg or (result,remaining input))

data Buf a = B {cur,end:: !Int, buf:: !(UArray Int a)}

parse (P p) ts =
    case p (B lo hi ts) of
      Right (x,B cur end _) ->
	  if cur==end+1
	  then Right x
	  else Left $ "trailing garbage at pos "++show cur -- !!
      Left msg -> Left msg
  where (lo,hi) = bounds ts

tstparse (P p) ts = p ts

un (P p) = p

unit :: a->Parser t a
unit x = P (\ts->Right (x,ts))

failP msg = P (\ts->Left msg)

bind :: Parser t a -> (a -> Parser t b) -> Parser t b
bind (P ap) atobp= P (cont.ap)
  where cont r =
          case r of
            Right (a,ts') -> un (atobp a) ts'
            Left msg -> Left msg

mapP :: (a->b) -> Parser t a -> Parser t b
mapP f (P ap) = P (\ts -> case ap ts of
			    Right (a,ts') -> Right (f a,ts')
			    Left msg -> Left msg)

ap :: Parser t (a->b)->Parser t a->Parser t b
ap abp ap = abp `bind` (`mapP` ap)

chk :: Parser t b->Parser t c->Parser t b
chk bp cp = const `mapP` bp `ap` cp

orelse :: Parser t a -> Parser t a -> Parser t a
orelse (P a1p) (P a2p) =
  P (\ts->case a1p ts of
	    Left msg1 ->
	      case a2p ts of
	        a@(Right _) -> a
		Left msg2 -> Left (msg1 ++ "\n" ++ msg2)
	    a1 -> a1)

--token :: Parser t t
token :: IArray UArray t => Parser t t
token = P tokenp
  where tokenp (B cur end ts) =
	    if cur<=end
	    then Right (ts!cur,B (cur+1) end ts)
	    else Left "unexpected end of file"

tokens n = P tokensp
  where tokensp (B cur end ts) =
	  if cur+n<=end
	  then Right ([ts!i|i<-[cur..cur+n-1]],B (cur+n) end ts)
	  else Left ("wanted "++show n++" bytes, got only "++show (end-cur+1))

--peek :: Parser t t
peek :: IArray UArray t => Parser t t
peek = P peekp
  where peekp b@(B cur end ts) =
	    if cur<=end
	    then Right (ts!cur,b)
	    else Left "unexpected end of file"

{-
eof :: Parser t ()
eof = P eofp
  where eofp [] = (Right ((),[]),[])
        eofp ts = (Left "end of file expected",ts)
-}

--theRest :: Parser t [t]
theRest :: IArray UArray t => Parser t [t]
theRest = P therest
  where therest (B cur end ts) =
	  Right ([ts!i|i<-[cur..end]],B (end+1) end ts)

--scan :: (t->Bool) -> Parser t t
scan p = scan' "unexpected token" p
scan' msg p = 
  do t <- token
     if p t
	then return t
	else failP msg

lit' t = scan (t==)
lit t = scan' ("expected "++show t)  (t==)

lits [] = unit []
lits (t:ts) = unit (:) `ap` lit t `ap` lits ts

many p = some p `orelse` unit []

some p = unit (:) `ap` p `ap` many p

cut = id -- ??

repeatP 0 p = unit []
repeatP n p = (:) `mapP` p `ap` repeatP (n-1) p

eitherP lP rP = mapP Left lP `orelse` mapP Right rP

----
instance Monad (Parser t) where
  (>>=) = bind
  return = unit

instance Functor (Parser t) where
  fmap = mapP
