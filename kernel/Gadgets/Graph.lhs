> module Graph where
> import Prelude hiding (Maybe(..))
> import Gadgets
> import Tree

> control :: Tree () -> In [Int] -> Out (Tree ()) -> Component
> control t i o =
>     claim i $
>     tx o t $
>     c t 
>     where
>     c t =
>         rx [
>             from i $ \m ->
>                 let t' =  newTree m t in
>                 tx o t' $
>                 c t'
>         ] (rxFail "control")

> annotate :: Tree () -> Tree ([Int],(Int,Int)) 
> annotate = addCoords.addAddr []

> whichnode x y (Leaf _ (a,(nx,ny))) = 
>     let (ax,ay) = sc (nx,ny) in
>     if dif x ax < 15 && dif y ay < 15 then Yes a else None
> whichnode x y (Node _ (a,(nx,ny)) l r) =
>     let (ax,ay) = sc (nx,ny) in
>     if dif x ax < 15 && dif y ay < 15 then Yes a
> 	else case whichnode x y l of
> 	    None -> whichnode x y r
> 	    Yes a -> Yes a

> dif a b = max a b - min a b

> sc (x,y) = (x*10,y*20+50)

> treeman :: In (Tree ()) -> Out [Int] -> Gadget
> treeman i o =
>     initGadget (400,400) blank $
>     claim i $
>     tm None
>     where
>     tm :: Maybe (Tree ([Int],(Int,Int))) -> Gadget
>     tm mt =
> 	rx [
> 	    from i $ \t ->
> 		let t' = annotate t in
> 		txSM (SMDrawFun (tree2df t')) $
> 		tm (Yes t'),
> 	    fromSM $ \r -> case mt of
> 		None ->
> 		    tm None
> 		Yes t -> case r of
> 		    SMMouseClick x y b ->
> 			let f = whichnode x y t in
> 			case f of
> 			    None ->
> 				tm mt
> 			    Yes m ->
> 				tx o m $
> 				tm mt
> 		    otherwise ->
> 			tm mt
> 	] (rxFail "tm")
>     tree2df :: Tree (a,(Int,Int)) -> DrawFun
>     tree2df t (x,y) _ = [colour "white",fillbox ((0,0),(x-1,y-1))]++tree2df' t
>     tree2df' (Node n (_,(x',y')) l r) = 
>         let (x,y) = sc (x',y') in
>         [colour "black",drawline ((x,y),(sc.posOf) l),drawline ((x,y),(sc.posOf) r),
>         colour "light grey",DrawFilledCircle ((x,y),15),
>         colour "black",DrawCircle ((x,y),15)]++
>         textat fn (x,y) (show n)++tree2df' l++tree2df' r
>     tree2df' (Leaf n (_,(x',y'))) = 
>         let (x,y) = sc (x',y') in 
>         [colour "light grey",DrawFilledCircle ((x,y),15),
>         colour "black",DrawCircle ((x,y),15)]++
>         textat fn (x,y) (show n)
>     posOf (Leaf _ (_,p)) = p
>     posOf (Node _ (_,p) _ _) = p
>     fn="*lucida-medium-r-*-120-*"

> gedit = (gedit',"gedit")
> gedit' :: Gadget
> gedit' =
>     wire $ \dc ->
>     wire $ \cd ->
>     spawn (control initTree (ip dc) (op cd)) $
>     treeman (ip cd) (op dc)
