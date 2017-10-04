import Html exposing (text)
import List
import Tuple exposing (first, second)

type Interval = Interval Float Float
type alias DayCover = List (Interval, Float)
type alias Table = List DayCover

(=>) = (,)

typicalKidsDay =
   [ Interval 7 9 => 12
   , Interval 9 15 => 24
   , Interval 15 17 => 12
   ]

kidsPresent = List.repeat 5 typicalKidsDay

typicalAdultsDay
 = [ Interval 9 15 => 4 ]

adultsPresent = List.repeat 5 typicalAdultsDay

normativeDay = [Interval 7 17 => 6] -- barn / voksen
normativeKidsPerAdult = List.repeat 5 normativeDay

lunchBreaks =
 [ Interval 9.5 10 => 1
 , Interval 11.75 12.25 => 1
 , Interval 13 13.5 => 2
 ]

main =
  Html.div []
  [ viewWeek normativeKidsPerAdult "Normert antall barn per voksen for Troll"
  , Html.hr [] []
  , viewWeek adultsPresent "Antall voksne i området. Troll"
  ]

viewWeek week caption =
  Html.div []
  [ Html.p [] [text caption]
  , Html.ol []
    <| List.map (Html.li [] << (\e -> e :: []) << text << daySummary) week
  ]

showInterval interval = toString (first interval) ++ ": " ++ (toString (second interval))
daySummary intervals = String.concat <| List.intersperse ", " <| List.map showInterval intervals
