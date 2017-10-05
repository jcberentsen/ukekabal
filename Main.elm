import Model exposing (..)

import List
import Tuple exposing (first, second)
import Dict
import Element exposing (layout, column, row, el, paragraph, cell, text, h1, h2, grid)
import Element.Attributes exposing (spacing, px)
import Stylesheet exposing (stylesheet, MyStyles(..))

(=>) = (,)

main =
  layout stylesheet <| column None [spacing 16]
  [ viewWeek normativeKidsPerAdult "Normert antall barn per voksen for Troll"
  , viewWeek adultsWeek "Antall voksne i området"
  , viewWeekQuartiles kidsQuartiles "Antall barn tilstede"
  , viewWeekQuartiles adultsAvailable "Antall tilgjengelige voksne"
  , viewWeekQuartiles (List.map2 kidsPerAdult kidsQuartiles adultsAvailable) "Antall barn per voksen"
  , viewWeekQuartiles (List.map2 kidsPerAdultFilter kidsQuartiles adultsAvailable) "Mange barn per voksen"
  ]

viewWeek week caption =
  column None []
  [ text caption
  , column None []
      <| List.map (text << daySummary) week
  ]

cellForQuartile day (q,v) =
  cell
      { start = (q - 28, day)
      , width = 1
      , height = 1
      , content = el None [] (text <| toString v)
      }

dayCells day qs  =
  List.map (cellForQuartile day) <| Dict.toList qs 

viewWeekQuartiles weekQuartiles caption =
  column None []
  <|
  [ text caption
  , grid None []
      { columns = List.repeat 100 (px 20) 
      , rows = List.repeat 1 (px 16)
      , cells = List.concat <| List.indexedMap dayCells weekQuartiles
      }
  ]

viewQuartile (k,v) =
  el None [] <| text <| toString v

viewQuartiles qs = List.map viewQuartile <| Dict.toList qs

showInterval interval = toString (first interval) ++ ": " ++ (toString (second interval))

daySummary intervals = String.concat <| List.intersperse ", " <| List.map showInterval intervals
