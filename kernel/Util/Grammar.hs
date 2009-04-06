module Util.Grammar where
import Text.PrettyPrint

data Grammar
    = Empty
    | Alt Grammar Grammar
    | Seq Grammar Grammar
    | Many Grammar
    | Opt Grammar
    | Terminal String
    | Nonterminal String Grammar
    | Grammar :--- String

ppGrammar :: Grammar -> (Doc,[Doc])
ppGrammar p =
    case p of
      Empty -> (empty,[])
      Seq p1 p2 -> (m1<+>m2,a1++a2)
	where (m1,a1) = ppGrammar p1
	      (m2,a2) = ppGrammar p2
      Nonterminal s p ->
	  (text "<"<>text s<>text ">",(text s<+>text "="<+>m): a)
	where (m,a) = ppGrammar p
      p :--- s -> (m<+>text "--"<+>text s,a) where (m,a) = ppGrammar p
      Terminal s -> (text s,[])
      Opt p1 -> (brackets m,as)
	where (m,as) = ppGrammar p1
      Alt p1 p2 -> (vcat ms,concat as)
	where (ms,as) = unzip (map ppGrammar [p1,p2])
      Many p -> (braces m,a) where (m,a) = ppGrammar p
