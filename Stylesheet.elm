module Stylesheet exposing (..)

import Style exposing (StyleSheet, style)

(=>) = (,)

type MyStyles
 = None

stylesheet = Style.styleSheet
   [ style None []
   ]

