{-

  This file is released to the public domain.

  It requires UnicodeData.txt, which is available here:

  http://www.unicode.org/Public/UNIDATA/UnicodeData.txt

-}

module Main where

import Numeric
import Data.List
import System.IO

main :: IO ()
main =
    do cs <- readFile "UnicodeData.txt"
       h <- openFile "wctype.c" WriteMode
       hPutStrLn h "#include <wctype.h>"
       hPutStrLn h "static int search(wint_t* p, wint_t lo, wint_t hi, wint_t wc) {"
       hPutStrLn h "  wint_t med = (lo + hi) / 2;"
       hPutStrLn h "  if (p[med] == wc) return 1;"
       hPutStrLn h "  else if (lo == hi) return 0;"
       hPutStrLn h "  else if (p[med] < wc) return search(p, med, hi, wc);"
       hPutStrLn h "  else return search(p, lo, med, wc);"
       hPutStrLn h "}"
       let ds = map semiSplit (lines cs)
           ps = [ (typeP ["Ll", "Lu", "Lt", "Lm", "Lo", "Nd", "Nl", "No"], "alnum", [])
                , (typeP ["Ll", "Lu", "Lt", "Lm", "Lo"], "alpha", [])
                , (typeP ["Cc"], "cntrl", [])
                , (typeP ["Nd", "Nl", "No"], "digit", [])
                , (typeP ["Ll"], "lower", [])
                , (typeP ["Pc", "Pd", "Ps", "Pe", "Pi", "Pf", "Po"], "punct", [])
                , (typeP ["Lu"], "upper", [])
                , (typeP ["Zs", "Zl", "Zp"], "blank", ["0009", "000A", "000D"]) ]
           ts = map (\ (p, n, cs) -> (n, (nub.sort) (map (read.("0x"++)) (cs ++ map head (filter p ds))))) ps
           outC (n, cs) =
               do let m = length cs
                  hPutStrLn h
                      ("static wint_t table_" ++ n ++ "[" ++ show m ++ "] = { " ++
                       concat (intersperse "," (map (\ n -> "0x" ++ showHex n "") cs)) ++
                       " };")
                  hPutStrLn h ("int isw" ++ n ++ "(wint_t wc) {")
                  hPutStrLn h ("  return search(table_"++n++", 0, " ++ show m ++ ", wc);")
                  hPutStrLn h "}"
       mapM_ outC ts
       hClose h

typeP :: [String] -> [String] -> Bool
typeP ts (_:_:t:_) = t `elem` ts

semiSplit :: String -> [String]
semiSplit [] = []
semiSplit cs =
    case span ( /= ';') cs of (e, ';':rest) -> e : semiSplit rest
                              (e, [])       -> [e]

