module Kernel.Driver.Keyboard 
    ( launchKeyboardInterpreter
    , launchKeyboardDecoder    
    , KeyPress(..)
    , KModSide(..)
    , KMod(..)
    , Key(..)
    , KeyEvent(..)
    , unKeyPress
    ) where

{-P:
import Prelude hiding (putStrLn)
-}
import Control.Monad ( mplus )
import Data.Char
import Data.Maybe ( fromJust )
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word
import H.Monad(H)
import H.Concurrency
--import Kernel.Debug

-- Most of theses datatypes where borrowed from Richard Braakman's SDL bindings

data KeyPress = KeyPress (Set KMod) Key
  deriving (Eq)

instance Show KeyPress where
   showsPrec _ (KeyPress mods k) = showString "KeyPress " .
			           shows (Set.toList mods) .
				   showChar ' ' .
				   shows k

unKeyPress :: KeyPress -> Key
unKeyPress (KeyPress _ k) = k

data KModSide
    = LSide 
    | RSide
  deriving (Eq, Ord, Bounded, Enum, Show)

data KMod
    = Shift KModSide
    | Ctrl KModSide
    | Alt KModSide
    | Meta KModSide
    | NumLock
    | CapsLock
    | ModeMod
    | ReservedMod -- ?
  deriving (Eq, Ord, Show)

data Key 
    = Key Char   -- For keys in the printable ascii range
    | WorldKey Int -- ?
    | BackspaceKey
    | TabKey
    | ClearKey
    | ReturnKey
    | PauseKey
    | EscapeKey
    | DeleteKey
    | Keypad Char    -- 0 through 9, plus, minus, etc.
    | KeypadEnterKey
    | UpKey | DownKey | RightKey | LeftKey
    | InsertKey | HomeKey | EndKey | PageUpKey | PageDownKey
    | FKey Int
    | ModKey KMod
    | SuperKey KModSide
    | ScrollLockKey
    | ComposeKey | HelpKey | PrintKey | SysReqKey | BreakKey
    | MenuKey | PowerKey | WakeKey | EuroKey | UndoKey
    | OtherKey Int
    | UnknownKey
    deriving (Eq, Ord, Show)

data KeyEvent
    = KeyPressed Key
    | KeyReleased Key
    | UnknownKeyEvent [Word8]
    deriving (Show)

{- This code is *highly* inefficient: it use the PreludeList lookup function
   on a pretty large table, and often for nothing.
   Transforming the keycode sequence map into a DFA seems to best path to
   follow.
 -}
launchKeyboardDecoder :: Chan Word8 -> H (Chan KeyEvent)
launchKeyboardDecoder codeChan =
    do keyChan <- newChan
       let set = x86Set1
	   longestSequence = maximum $ map (length . fst) set
	   scanHandler cs =
	       do c <- readChan codeChan
		  let sequence = cs ++ [c]
		  case lookup sequence set
		       of Nothing ->
			    if length sequence >= longestSequence
			       then do --putStrLn "Unknown scan code sequence"
				       writeChan keyChan
						 (UnknownKeyEvent sequence)
				       scanHandler []
			       else scanHandler sequence
			  Just event -> do writeChan keyChan event
					   scanHandler []
       forkH $ scanHandler []
       return keyChan

launchKeyboardInterpreter :: Chan KeyEvent -> H (Chan KeyPress)
launchKeyboardInterpreter eventChan =
    do pressChan <- newChan
       let interpreter mods dead =
	       do event <- readChan eventChan
		  let (mods', dead', ks) = interpret mods dead event
		  mapM_ (writeChan pressChan) ks
		  interpreter mods' dead'
       forkH $ interpreter Set.empty Nothing
       return pressChan

type DeadKey = Char -- ?

{- Unpretty code either.
 -}
interpret :: Set KMod -> Maybe DeadKey -> KeyEvent
	  -> (Set KMod, Maybe DeadKey, [KeyPress])
interpret mods dead (KeyPressed (ModKey mod)) =
    (Set.insert mod mods, dead, [])
interpret mods dead (KeyPressed k@(Key c))
    = handleMapping $ fromJust $ 
	      (if Shift LSide `Set.member` mods || Shift RSide `Set.member` mods
               then lookup c shiftMap `mplus` lookup c normalMap
               else lookup c normalMap) `mplus` Just (Right c)
    where handleMapping (Left dead') =
	      case dead of Nothing -> (mods, Just dead', [])
			   Just c  ->
			       let ks = map (KeyPress mods . Key) [c, dead']
			        in (mods, Nothing, ks)
	  handleMapping (Right c) =
	      case dead of
	      Nothing -> (mods, Nothing, [KeyPress mods (Key c)])
			 -- FIXME!
	      Just dc -> (mods, Nothing, [KeyPress mods (Key dc)])

interpret mods dead (KeyPressed k) =
    (mods, dead, [KeyPress mods k])
interpret mods dead (KeyReleased (ModKey mod)) =
    (Set.delete mod mods, dead, [])
interpret mods dead _ =
    (mods, dead, [])

normalMap :: [(Char, Either DeadKey Char)]
normalMap =
    []

shiftMap :: [(Char, Either DeadKey Char)]
shiftMap =
    [ (c, Right (toUpper c)) | c <- ['a'..'z'] ]
    ++
    [ ('`', Right '~')
    , ('1', Right '!')
    , ('2', Right '@')
    , ('3', Right '#')
    , ('4', Right '$')
    , ('5', Right '%')
    , ('6', Right '^')
    , ('7', Right '&')
    , ('8', Right '*')
    , ('9', Right '(')
    , ('0', Right ')')
    , ('-', Right '_')
    , ('=', Right '+')
    , ('[', Right '{')
    , (']', Right '}')
    , (';', Right ':')
    , ('\'', Right '"')
    , ('\\', Right '|')
    , (',', Right '<')
    , ('.', Right '>')
    , ('/', Right '?')
    ]

type KeyboardSet = [([Word8], KeyEvent)]

x86Set1 :: KeyboardSet
x86Set1 = map (f KeyPressed) x86Set1Pressed
	  ++ map (f KeyReleased) x86Set1Released
    where f con (cs, k) = (cs, con k)

-- http://www.win.tue.nl/~aeb/linux/kbd/scancodes-9.html
x86Set1Released :: [([Word8], Key)]
x86Set1Released = foldl f [] x86Set1Pressed
    where f xs (scancodes, key) =
	      case scancodes
		   of 0xe0:c:[] -> ([0xe0, c + 0x80], key):xs
		      c:[]      -> ([c + 0x80], key):xs
		      _         -> xs

x86Set1Pressed :: [([Word8], Key)]
x86Set1Pressed =
    [ ([0x29], Key '`')
    , ([0x02], Key '1')
    , ([0x03], Key '2')
    , ([0x04], Key '3')
    , ([0x05], Key '4')
    , ([0x06], Key '5')
    , ([0x07], Key '6')
    , ([0x08], Key '7')
    , ([0x09], Key '8')
    , ([0x0a], Key '9')
    , ([0x0b], Key '0')
    , ([0x0c], Key '-')
    , ([0x0d], Key '=')
    , ([0x0e], BackspaceKey)
    , ([0x0f], TabKey)
    , ([0x10], Key 'q')
    , ([0x11], Key 'w')
    , ([0x12], Key 'e')
    , ([0x13], Key 'r')
    , ([0x14], Key 't')
    , ([0x15], Key 'y')
    , ([0x16], Key 'u')
    , ([0x17], Key 'i')
    , ([0x18], Key 'o')
    , ([0x19], Key 'p')
    , ([0x1a], Key '[')
    , ([0x1b], Key ']')
    , ([0x2b], Key '\\')
    , ([0x3a], ModKey CapsLock)
    , ([0x1e], Key 'a')
    , ([0x1f], Key 's')
    , ([0x20], Key 'd')
    , ([0x21], Key 'f')
    , ([0x22], Key 'g')
    , ([0x23], Key 'h')
    , ([0x24], Key 'j')
    , ([0x25], Key 'k')
    , ([0x26], Key 'l')
    , ([0x27], Key ';')
    , ([0x28], Key '\'')
--    , ([0x00], ?)
    , ([0x1c], ReturnKey)
    , ([0x2a], ModKey (Shift LSide))
    , ([0x2c], Key 'z')
    , ([0x2d], Key 'x')
    , ([0x2e], Key 'c')
    , ([0x2f], Key 'v')
    , ([0x30], Key 'b')
    , ([0x31], Key 'n')
    , ([0x32], Key 'm')
    , ([0x33], Key ',')
    , ([0x34], Key '.')
    , ([0x35], Key '/')
    , ([0x36], ModKey (Shift RSide))
    , ([0x1d], ModKey (Ctrl LSide))
    , ([0x38], ModKey (Alt LSide))
    , ([0x39], Key ' ')
    , ([0xe0, 0x38], ModKey (Alt RSide))
    , ([0xe0, 0x1d], ModKey (Ctrl RSide))
    , ([0xe0, 0x52], InsertKey)
    , ([0xe0, 0x53], DeleteKey)
    , ([0xe0, 0x47], HomeKey)
    , ([0xe0, 0x4f], EndKey)
    , ([0xe0, 0x49], PageUpKey)
    , ([0xe0, 0x51], PageDownKey)
    , ([0xe0, 0x4b], LeftKey)
    , ([0xe0, 0x48], UpKey)
    , ([0xe0, 0x50], DownKey)
    , ([0xe0, 0x4d], RightKey)
    , ([0x45], ModKey NumLock)
    , ([0x47], Keypad '7')
    , ([0x4b], Keypad '4')
    , ([0x4f], Keypad '1')
    , ([0xe0, 0x35], Keypad '/')
    , ([0x48], Keypad '8')
    , ([0x4c], Keypad '5')
    , ([0x50], Keypad '2')
    , ([0x52], Keypad '0')
    , ([0x37], Keypad '*')
    , ([0x49], Keypad '9')
    , ([0x4d], Keypad '6')
    , ([0x51], Keypad '3')
    , ([0x53], Keypad '.')
    , ([0x4a], Keypad '-')
    , ([0x4e], Keypad '+')
    , ([0xe0, 0x1c], KeypadEnterKey)
    , ([0x01], EscapeKey)
    , ([0x3b], FKey 1)
    , ([0x3c], FKey 2)
    , ([0x3d], FKey 3)
    , ([0x3e], FKey 4)
    , ([0x3f], FKey 5)
    , ([0x40], FKey 6)
    , ([0x41], FKey 7)
    , ([0x42], FKey 8)
    , ([0x43], FKey 9)
    , ([0x44], FKey 10)
    , ([0x57], FKey 11)
    , ([0x58], FKey 12)
    , ([0xe0, 0x37], PrintKey)
    , ([0x54], SysReqKey)
    , ([0x46], ScrollLockKey)
    , ([0xe1, 0x1d, 0x45, 0xe1, 0x9d, 0xc5], PauseKey)
    , ([0xe0, 0x46], BreakKey)
    , ([0xe0, 0x5b], ModKey (Meta LSide))
    , ([0xe0, 0x5c], ModKey (Meta RSide))
    , ([0xe0, 0x5d], MenuKey)
    , ([0xe0, 0x5e], PowerKey)
    , ([0xe0, 0x63], WakeKey)
    ]
