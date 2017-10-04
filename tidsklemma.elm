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
 = [ Interval 7 8 => 1
   , Interval 8 8.75 => 2
   , Interval 8.75 9.5 => 3
   , Interval 9.5 14.25 => 4
   , Interval 14.25 15.5 => 3
   , Interval 15.5 16.25 => 2
   , Interval 16.25 17 => 1
   ]

shortStaffed
 = [ Interval 7 8 => 1
   , Interval 8 8.75 => 2
   , Interval 8.75 9.5 => 3 -- her gaar noen hjem
   , Interval 9.5 13.5 => 4
   , Interval 13.5 15.5 => 3
   , Interval 15.5 16.25 => 2
   , Interval 16.25 17 => 1
   ]

adultsWeek = shortStaffed :: (List.repeat 4 typicalAdultsDay)

normativeDay = [Interval 7 17 => 6] -- barn / voksen
normativeKidsPerAdult = List.repeat 5 normativeDay

temporaryAbsense =
 [ Interval 9.5 10 => 1 -- lunch
 , Interval 11.75 12.25 => 1 --lunch
 , Interval 13 13.5 => 2 --lunch
 ]

absenseWeek = List.repeat 5 temporaryAbsense

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

kidsQuartiles = List.map dayToQuartiles kidsPresent

kidsPerAdult kids adults = 
    Dict.merge
        (\k kid r -> Dict.insert k 9999 r)
        (\k kid adult r -> Dict.insert k (kid // adult) r)
        (\k adult r -> Dict.insert k 0 r)
        kids adults
        Dict.empty

kidsPerAdultFilter kids adults = 
    Dict.merge
        (\k kid r -> Dict.insert k 9999 r)
        insertKpaIfHigh
        (\k adult r -> Dict.insert k 0 r)
        kids adults
        Dict.empty

insertKpaIfHigh k kid adult r =
    let kpa = kid // adult
    in
    if kpa > 6 then Dict.insert k (kid // adult) r else r

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

adultsAvailable = subtract adultsWeek absenseWeek

main =
  Html.div []
  [ viewWeek normativeKidsPerAdult "Normert antall barn per voksen for Troll"
  , Html.hr [] []
  , viewWeek adultsWeek "Antall voksne i området"
  , viewWeekQuartiles kidsQuartiles "Antall barn tilstede"
  , viewWeekQuartiles adultsAvailable "Antall tilgjengelige voksne"
  , viewWeekQuartiles (List.map2 kidsPerAdult kidsQuartiles adultsAvailable) "Antall barn per voksen"
  , viewWeekQuartiles (List.map2 kidsPerAdultFilter kidsQuartiles adultsAvailable) "Mange barn per voksen"
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
