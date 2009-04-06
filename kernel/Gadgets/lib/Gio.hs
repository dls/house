module Gio where
import Control.Concurrent(Chan)
import Data.Word(Word8,Word32)
import Data.Array.Unboxed(UArray)

type GadgetDisplay = Chan GCommand -> Chan GEvent -> IO ()

data GEvent = ME MouseEvnt | KE KeyboardEvnt | SE ScreenEvnt
    deriving ({-Read,-}Show)
data GCommand = MC MouseCmnd | KC KeyboardCmnd | SC [ScreenCmnd]
    deriving ({-Read,-}Show)

data MouseCmnd 
    = MouseReportClicks Bool
    | MouseReportMovement Bool
    deriving ({-Read,-}Show)
data MouseEvnt 
    = MouseClick Int Int Int
    | MouseUnClick Int Int Int
    | MouseMove Int Int
    deriving ({-Read,-}Show)
data KeyboardCmnd
    = KeyboardRepeat Int
    deriving ({-Read,-}Show)
data KeyboardEvnt
    = KeyPress Char
    deriving ({-Read,-}Show)
type PixelValue = Word32
type ColorMap = UArray Word8 PixelValue
data Pixmap
    = Pixmap ColorMap (Maybe Word8) (Int,Int) (UArray Int Word8)
         -- cmap transparency (width,height) pxls
    deriving ({-Read,-}Show)
data ScreenCmnd 
    = DrawSetPixel (Int,Int) PixelValue
    | DrawLine ((Int,Int),(Int,Int))
    | DrawFilledRectangle ((Int,Int),(Int,Int))
    | DrawPixmap Pixmap (Int,Int)
    | DrawSetColour (Int,Int,Int)
    | DrawSetPixelValue PixelValue
    | DrawSetClip ((Int,Int),(Int,Int))
    | DrawText (Int,Int) String
    | DrawSetFont String
    | DrawFilledTriangle ((Int,Int),(Int,Int),(Int,Int))
    | DrawCircle ((Int,Int),Int)
    | DrawFilledCircle ((Int,Int),Int)
    deriving ({-Read,-}Show)
data ScreenEvnt 
    = Expose ((Int,Int),(Int,Int))
    deriving ({-Read,-}Show)
