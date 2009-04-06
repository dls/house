module Kernel.GrubModules
  ( moduleName,moduleNames,findModule
  , moduleCount,moduleRegion
  , pickModule, pickModule'
  , Err(..)
  ) where

import Data.List(tails,isPrefixOf)
import H.Grub



moduleNames :: H [String]
moduleNames =
  do n <- moduleCount
     mapM moduleName [0..n-1]	  

findModule :: String -> H (Maybe Int)
findModule name =
  do names <- moduleNames
     let nn = zip names [(0::Int)..]
     return (lookup name nn)

pickModule' :: String -> H (Either Err Int)
pickModule' s 
  = do ms <- moduleNames
       case [m | m<-ms,s `isSubsequenceOf` m] of
         [m] -> do ma <- findModule m
                   return $
                     case ma of
                       Nothing -> Left (NotFound s)
                       Just a  -> Right a
         []  -> return (Left (NotFound s))
         ms  -> return (Left (AmbigPath s ms))



pickModule :: (String -> H ()) -> String -> H (Maybe Int)
pickModule putStrLn s 
  = do ms <- moduleNames
       case [m | m<-ms,s `isSubsequenceOf` m] of
         [m] -> findModule m
         []  -> return Nothing
         ms   -> do putStrLn $ "Ambiguous subpath: "++unwords ms
                    return Nothing

s `isSubsequenceOf` l = any (s `isPrefixOf`) (tails l)


data Err  = NotFound String
          | AmbigPath String [String]

instance Show Err where
  show (NotFound x)     = "Cannot find module " ++ show x
  show (AmbigPath x xs) = "The path " ++ show x ++ " is ambiguous:\n" 
                       ++ " *** It could refer to: " ++ show xs

