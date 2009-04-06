module Keys(keys) where
--import Prelude --hiding (Maybe(..))
import GadgetsPrelude hiding (Maybe(..))
import Components


keys :: Out String -> Gadget
keys o =
    initGadget (20,20) blankbg $
    txSM SMClaimFocus $
    tm True
  where

    tm :: Bool -> Gadget
    tm focus =
	rx [fromSM $ \r ->
              case r of
                SMKeyPress c -> tx o [c] same
	        SMMouseClick{} ->
	            if focus
		    then same
		    else txSM SMClaimFocus $ changeFocus True
	        SMLoseFocus -> changeFocus False
		_ -> same
	] (rxFail "tm")
      where
        same = tm focus
	newtxt = tm focus
	changeFocus focus' = tm focus'

    blankbg = colourbox (col "paper")
