module Geometry.Cube
  ( volume,
    area,
  )
where

volume :: Float -> Float
volume side = side ^ 3

area :: Float -> Float
area side = 6 * (side ^ 2)
