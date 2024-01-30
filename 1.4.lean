#check 1.2

structure Point where
  x : Float
  y : Float
deriving Repr

def origin : Point := { x := 0, y := 0 }
#eval origin
#eval origin.x
#eval origin.y

def addPoints (p1 p2 : Point) : Point :=
  { x := p1.x + p2.x, y := p1.y + p2.y }

def distance (p1 p2 : Point) : Float :=
  Float.sqrt (((p2.x - p1.x) ^ 2.0) + ((p2.y - p1.y) ^ 2.0))

#eval distance { x := 1.0, y := 2.0 } { x := 5.0, y := -1.0 }

structure Point3D where
  x : Float
  y : Float
  z : Float
deriving Repr

def origin3D : Point3D := { x := 0, y := 0, z := 0 }

#check ({ x := 0, y := 0} : Point)
#check { x := 1, y := 2 : Point}

def zeroX (p : Point) : Point :=
  { x := 0, y := p.y }

def zeroX' (p : Point) : Point :=
  { p with x := 0 }

def fourAndThree : Point :=
  { x := 4.3, y := 3.4 }

-- mk constructor
#check Point.mk 1.5 2.8
#check (Point.mk) -- constructor is a function

structure Point' where
  point :: -- rename constructor
  x : Float
  y : Float
deriving Repr

#check (Point'.point)

-- accessor functions
#check (Point.x)
#eval Point.x origin

#eval "one string".append " and another"

def Point.mofidyBoth (f : Float -> Float) (p : Point) : Point :=
  { x := f p.x, y := f p.y }

#eval fourAndThree.mofidyBoth Float.floor

-- ### Exercises

-- Define a structure named RectangularPrism that contains the height, width, and depth of a rectangular prism, each as a Float.
structure RectangularPrism where
  height : Float
  width : Float
  depth : Float
deriving Repr

-- Define a function named volume : RectangularPrism → Float that computes the volume of a rectangular prism.
def volume (r : RectangularPrism) : Float :=
  r.depth * r.height * r.width

-- Define a structure named Segment that represents a line segment by its endpoints, and define a function length : Segment → Float that computes the length of a line segment. Segment should have at most two fields.
structure Segment where
  a : Point
  b : Point
deriving Repr

def Segment.length (s : Segment) : Float :=
  distance s.a s.b
