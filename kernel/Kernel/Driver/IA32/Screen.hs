-- STILL NEEDS SOME CONVERSION WORK TO MATCH PAPER

module Kernel.Driver.IA32.Screen
    ( launchConsoleDriver
    ) where

import Control.Monad ( when )
import Data.Char ( ord )

import Data.Word
import Data.Bits
import Kernel.Types.Console
import H.Monad(H)
import H.Concurrency(forkH,newChan,readChan,newMVar)
import H.IOPorts
import H.AdHocMem(absolutePtr)
import H.MemRegion

screen :: MemRegion
screen = createRegion (absolutePtr 0xB8000)
	              (toEnum (2*screenHeight*screenWidth))

{- From: http://www.openbg.net/sto/os/xml/console_io.html -}
registerIndexPort :: Port
registerIndexPort = 0x3D4

dataPort :: Port
dataPort = 0x3D5

defaultAttrs :: Word8
defaultAttrs = 0x17

screenHeight = 25
screenWidth = 80

isInvalidPosition :: Row -> Col -> Bool
isInvalidPosition row col =
    row < 0 || col < 0 || row >= screenHeight || col >= screenWidth

clearScreen :: H ()
clearScreen =
    sequence_ [ writeChar r c defaultAttrs ' '
		| r <- [0..screenHeight - 1], c <- [0..screenWidth - 1] ]

writeChar :: Row -> Col -> VideoAttributes -> Char -> H ()
writeChar row col attr c
    | isInvalidPosition row col =
	fail "writeChar: invalid screen position"
    | otherwise = pokeElemOff screen (fromIntegral ((row * screenWidth) + col)) w
    where
      w :: Word16
      w = (fromIntegral attr `shiftL` 8) .|. fromIntegral (ord c)

moveCursor :: Row -> Col -> H ()
moveCursor row col
    | isInvalidPosition row col =
	fail $ "moveCursor: invalid screen position ("
		 ++ show row ++ ", " ++ show col ++ ")"
    | otherwise =
	let coord :: Word32
	    coord = fromIntegral (fromIntegral row * screenWidth + col)
	 in do -- Put high bytes first
	       outB registerIndexPort 14
	       outB dataPort $ fromIntegral $ shiftR coord 8
	       -- Put low bytes then
	       outB registerIndexPort 15
	       outB dataPort $ fromIntegral coord

--scrollUp :: H ()
scrollUp =
  do moveScreenBytes (screenWidth*2) -- destination, don't scroll first line
	             (screenWidth*4) -- source
		     (screenWidth*(screenHeight-2)*2) -- byte count
     sequence_ [ writeChar (screenHeight - 1) c defaultAttrs ' '
		 | c <- [0..screenWidth - 1] ]
  where
    moveScreenBytes dst src n =
        moveBytes screen (toEnum dst) screen (toEnum src) (toEnum n)

--launchConsoleDriver :: H Console
launchConsoleDriver =
    do chan <- newChan
       --clearScreen
       let consolePrinter row col =
	       do command <- readChan chan
		  (row', col') <- doCommand command row col >>= adjustPosition
		  moveCursor row' col'
		  consolePrinter row' col'
	   adjustPosition (row, col)
	       | row >= screenHeight || col >= screenWidth =
		   doCommand NewLine row col >>= adjustPosition
	       | otherwise = return (row, col)
       forkH $ consolePrinter 20 0
       vConsole <- newMVar $ ConsoleData { consoleChan = chan
					 , consoleHeight = fromEnum screenHeight
					 , consoleWidth = fromEnum screenWidth
					 }
       return (Console vConsole)

doCommand :: ConsoleCommand -> Row -> Col -> H (Row, Col)
doCommand NewLine row _
    | row >= screenHeight = do scrollUp
			       return (row - 1, 0)
    | otherwise = return (row + 1, 0)
doCommand CarriageReturn row _ =
    return (row, 0)
doCommand (PutChar attrs c) row col =
    do writeChar row col attrs c
       return (row, col + 1)
doCommand (MoveCursorBackward count) row col =
    let (q, r) = (col - count) `divMod` screenWidth
    in return $ max (0, 0) (row + q, r)
doCommand ClearEOL row col =
    do sequence_ [ writeChar row c defaultAttrs ' ' | c <- [col..screenWidth-1]]
       return (row,col)       
doCommand ClearScreen _ _ =
    do clearScreen
       return (0, 0)
