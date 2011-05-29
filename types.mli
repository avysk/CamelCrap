type point = float * float
type line = point * point
type polygon = point array
type circle = point * float
type ellipse = point * point * float

type figure =
  | L of line
  | P of polygon
  | C of circle
  | E of ellipse

type thing =
  | Number of float
  | String of string
  | Point of point
  | Figure of figure

type cmd_code =
  | Num of float
  | Id of string
  | Plus
  | Minus
  | Multiply
  | Divide
  | PointPlus
  | PointMinus
  | PointScaleUp
  | PointScaleDown
  | MakePoint
  | Assign
  | Line of thing list
  | Circle of thing list
  | Ellipse
  | MakeCircle
  | MakeEllipse
  | MakeNgon
  | Distance
  | Trope
  | Pwalk
  | Pspin
  | Ngonloop of (thing * thing * thing * thing)
  | Pop
  | PrintDictionary
  | PrintStack
  | LoopStart
