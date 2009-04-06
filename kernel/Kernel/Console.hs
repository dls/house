module Kernel.Console
    ( Console
    , putString, putStringLn
    , Kernel.Console.putChar, putChar'
    , clearScreen
    , moveCursorBackward
    , clearEOL
    ) where

{-P:
import Prelude hiding (putChar)
-}
import H.Monad(H)
import H.Concurrency
import Control.Monad ( when )

import Kernel.Types.Console

defaultAttrs :: VideoAttributes
defaultAttrs = 0x17

putString :: Console -> String -> H ()
putString (Console vConsole) str =
    withMVar vConsole $ \ console ->
	writeList2Chan (consoleChan console) $ map putc str

putc '\n' = NewLine
putc c = PutChar defaultAttrs c

putStringLn :: Console -> String -> H ()
putStringLn (Console vConsole) str =
    withMVar vConsole $ \ con ->
	do writeList2Chan (consoleChan con) $ map putc str
	   writeChan (consoleChan con) NewLine

putChar :: Console -> Char -> H ()
putChar (Console vConsole) char =
    withMVar vConsole $ \ console ->
	do writeChan (consoleChan console) $ putc char

putChar' :: Console -> VideoAttributes -> Char -> H ()
putChar' (Console vConsole) attrs char =
    withMVar vConsole $ \ console ->
	do writeChan (consoleChan console) $ PutChar attrs char

clearScreen :: Console -> H ()
clearScreen (Console vConsole) =
    withMVar vConsole $ \ console ->
	do writeChan (consoleChan console) $ ClearScreen

moveCursorBackward :: Console -> Int -> H ()
moveCursorBackward (Console vConsole) count =
    withMVar vConsole $ \ console ->
	writeChan (consoleChan console) $ MoveCursorBackward count

clearEOL :: Console -> H ()
clearEOL (Console vConsole) =
    withMVar vConsole $ \ console ->
	writeChan (consoleChan console) ClearEOL

isValidPosition :: ConsoleData -> Row -> Col -> H Bool
isValidPosition con row col =
    let height = consoleHeight con
	width = consoleWidth con
     in return (row < 0 || col < 0 || row >= height || col >= width)

checkPosition :: ConsoleData -> Row -> Col -> H ()
checkPosition console row col =
    do valid <- isValidPosition console row col
       when (not valid) $ fail "Invalid position"
