module Structures.Composers
    (
        ($+$),
        ($:$)
    )

where

import           Structures.Lines
import           Structures.Words

($+$) :: OneWord -> OneWord -> Line
l $+$ r = Line [l, r]
infixr 5 $+$

($:$) :: OneWord -> Line -> Line
l $:$ Line lst  = Line  $ l:lst
_ $:$ b  = b -- todo: rewrite the composition
infixr 5 $:$
