
-- | A simple library for representing shapes.
module Shape where

type Radius = Float
type Length = Float
type Side = Float
type Height = Float
type Width = Float

data Shape = Circle Radius
           | Square Length
           | Triangle Side Side Side
           | Rectangle Height Width
  deriving (Eq,Show)

heron :: Side -> Side -> Side -> Float
heron a b c = sqrt (s * (s - a) * (s - b) * (s - c))
	where s = (a + b + c) / 2

-- | Compute the area of a shape.
area :: Shape -> Float
area shape = case shape of
             Circle r -> pi * r ^ 2
             Square l -> l ^ 2
             Triangle a b c -> heron a b c
             Rectangle l w -> l * w

-- | Compute the perimiter of a shape.
perimeter :: Shape -> Float
perimeter shape = case shape of
                  Circle r -> 2 * pi * r
                  Square l -> l * 4
                  Triangle a b c -> a + b + c
                  Rectangle l w -> 2 * (l + w)