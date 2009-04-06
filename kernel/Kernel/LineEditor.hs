module Kernel.LineEditor
  ( LineEditor
  , newEditor
  , getLine
  ) where

{---
 Loosely based on SimpleLineEditor by Malcom Wallace
 http://www.haskell.org/pipermail/glasgow-haskell-users/2003-June/005370.html
---}
{-P:
import Prelude hiding (getLine)
-}
import H.Monad(H)
import H.Concurrency
import Control.Monad(unless)
import Data.Set (member)

import Kernel.Console
import Kernel.Driver.Keyboard ( KeyPress(..), Key(..), KMod(..), KModSide(..) )

data LineEditorData = LineEditorData
    { editorChan :: Chan KeyPress
    , editorConsole :: Console
    , editorHistory :: MVar [String]
    }

data LineEditor = LineEditor (MVar LineEditorData)

data LineCmd
    = Char Char
    | Move Cursor
    | Delete Cursor
    | Accept
    | History
    | Future
    | Clear
    | NoOp

data Cursor
    = Previous 
    | Next
    | PreviousWord
    | NextWord
    | Begin
    | End

newEditor :: Chan KeyPress -> Console -> H LineEditor
newEditor chan console =
    do vHistory <- newMVar []
       vEditor <- newMVar $ LineEditorData { editorChan = chan
					   , editorConsole = console
					   , editorHistory = vHistory
					   }
       return $ LineEditor vEditor

inverseVideo = 0x71

getLine :: LineEditor -> String -> H String
getLine (LineEditor vEditor) prompt =
    withMVar vEditor $ \ editor ->
	do let con = editorConsole editor
	       gl (before, after) len hist =
	           do putString con before
		      case after of []   -> do putChar' con inverseVideo ' '
					       clearEOL con
					       moveCursorBackward con 1
			            c:cs -> do putChar' con inverseVideo c
					       putString con cs
					       clearEOL con
		      moveCursorBackward con len
		      key <- readChan (editorChan editor)
		      case translateKey key of
		        Char c ->
			    gl (before ++ [c], after) (len + 1) hist
			Accept ->
			    do putString con before
			       putString con after
			       clearEOL con
			       return (before ++ after)
			Delete Previous ->
			    if null before
			       then gl (before, after) len hist
			       else gl (init before, after) (len - 1) hist
			Delete Begin ->
			    gl ([], after) (len - length before) hist
			Delete Next ->
			    case after of
			      []   -> gl (before, after) len hist
			      _:cs -> gl (before, cs) (len - 1) hist
			Delete End ->
			    gl (before, []) (len - length after) hist
			Move Previous ->
			    if null before
			       then gl (before, after) len hist
			       else gl (init before, (last before):after)
				       len hist
			Move Begin ->
			    gl ([], before ++ after) len hist
			Move Next ->
			    case after of
			      []   -> gl (before, after) len hist
			      c:cs -> gl (before ++ [c], cs) len hist
			Move End ->
			    gl (before ++ after, []) len hist
			History ->
			    case hist of
			      (fut, []) -> gl (before, after) len hist
			      (fut, p:past) -> gl (p, []) (length p)
					          ((before ++ after):fut, past)
			Future ->
			    case hist of
			      ([], past) -> gl (before, after) len hist
			      (f:fut, past) -> gl (f, []) (length f)
					          (fut, (before ++ after):past)
			Clear ->
			    do clearScreen con
			       putString con prompt
			       gl (before, after) len hist
			_ -> gl (before, after) len hist
           putString con prompt
	   history <- readMVar (editorHistory editor)
           line <- gl ([], []) 0 ([], history)
	   unless (null line) $ modifyMVar_ (editorHistory editor) $
	       return . (line:)
	   return line

translateKey :: KeyPress -> LineCmd
translateKey (KeyPress modSet key) =
    case key of
      Key c ->
	  if (Ctrl LSide) `member` modSet || (Ctrl RSide) `member` modSet
	     then case c of
		    'a' -> Move Begin
		    'e' -> Move End
		    'k' -> Delete End
		    'u' -> Delete Begin
		    'h' -> Delete Previous
		    'd' -> Delete Next
		    'j' -> Accept
		    'l' -> Clear
		    _   -> NoOp
	     else Char c
      Keypad c -> Char c
      ReturnKey -> Accept
      KeypadEnterKey -> Accept
      BackspaceKey -> Delete Previous
      DeleteKey -> Delete Next
      UpKey -> History
      DownKey -> Future
      LeftKey -> Move Previous
      RightKey -> Move Next
      HomeKey -> Move Begin
      EndKey -> Move End
      TabKey -> NoOp -- TODO: Completion !
      _ -> NoOp
