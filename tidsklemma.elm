import Html exposing (text)
import List
import Tuple exposing (first, second)
import Dict

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
 = [ Interval 9 14.25 => 4
   , Interval 14.25 15.5 => 3
   , Interval 15.5 16.25 => 2
   , Interval 16.25 17 => 1
   ]

adultsPresent = List.repeat 5 typicalAdultsDay

normativeDay = [Interval 7 17 => 6] -- barn / voksen
normativeKidsPerAdult = List.repeat 5 normativeDay

lunchBreaks =
 [ Interval 9.5 10 => 1
 , Interval 11.75 12.25 => 1
 , Interval 13 13.5 => 2
 ]

lunchWeek = List.repeat 5 lunchBreaks

quartile time = floor <| 4 * time

dayToQuartiles day =
    let quarts = List.map intervalToQuartiles day
    in
    List.foldr Dict.union Dict.empty quarts

intervalToQuartiles ((Interval from to), val) =
  let fromQuart = quartile from
      toQuart = quartile to
  in
    Dict.fromList <| List.map (\q -> (q, val)) (List.range fromQuart toQuart)

subtract all some =
    List.map2 daySubtract all some

daySubtract all some =
    let from = dayToQuartiles all
        take = dayToQuartiles some
    in
    Dict.merge
        (\k a r -> Dict.insert k a r)
        (\k a b r -> Dict.insert k (a-b) r)
        (\k b r -> r)
        from take
        Dict.empty

adultsAvailable = subtract adultsPresent lunchWeek

main =
  Html.div []
  [ viewWeek normativeKidsPerAdult "Normert antall barn per voksen for Troll"
  , Html.hr [] []
  , viewWeek adultsPresent "Antall voksne i området"
  , viewWeekQuartiles adultsAvailable "Antall tilgjengelige voksne"
  , viewQuartiles <| intervalToQuartiles <| (Interval 9 14.25 => 4)
  ]

viewWeek week caption =
  Html.div []
  [ Html.p [] [text caption]
  , Html.ol []
    <| List.map (Html.li [] << List.singleton << text << daySummary) week
  ]

viewWeekQuartiles weekQuartiles caption =
  Html.div []
  [ Html.p [] [text caption]
  , Html.ol []
    <| List.map (Html.li [] << List.singleton << viewQuartiles) weekQuartiles
  ]

viewQuartiles = toString >> text >> List.singleton >> Html.p []

showInterval interval = toString (first interval) ++ ": " ++ (toString (second interval))
daySummary intervals = String.concat <| List.intersperse ", " <| List.map showInterval intervals
