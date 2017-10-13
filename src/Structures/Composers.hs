module Structures.Composers
    (
        strToLine
    )

where

import           Structures.Lines (Line (Line))
import           Structures.Words (OneWord (RegWord))
{-
($+$) :: OneWord -> OneWord -> Line
l $+$ r = Line [l, r]
infixr 5 $+$

($:$) :: OneWord -> Line -> Line
l $:$ Line lst  = Line  $ l:lst
_ $:$ b  = b -- todo: rewrite the composition
infixr 5 $:$
-}
{-
Only use this function if you are sure that the string indeed contains nothing but regwords,
 it has no input validation
-}
strToLine :: String -> Line
strToLine = Line . fmap RegWord . words
