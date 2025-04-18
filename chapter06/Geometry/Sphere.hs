module Geometry.Sphere
  ( volume,
    area,
  )
where

volume :: Float -> Float
volume r = (4 / 3) * pi * (r ^ 3)

area :: Float -> Float
area r = 4 * pi * (r ^ 2)
