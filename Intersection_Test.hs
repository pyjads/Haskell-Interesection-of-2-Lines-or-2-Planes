import Geometry


-- command to execute in Haskell compiler to load the script
-- :load "Intersection_Test.hs"

-- To display and compute line intersection output use below command
-- line

-- To display and compute plane intersection output use below command
-- plane

---------------------------------------------------------------------------------------------------------------------------------------


-- Line Intersection
-- Please provide input in line_1 and line_2 for line intersection and use command 'line' (without quotes) in console to display line intersection output

line_1:: Geometry
line_1 = Line 1 2 3 -- ax + by +c = 0 here for line_1 = Line 1 0 (-8) --> a = 1, b = 0, c = -8 

line_2:: Geometry
line_2 = Line 3 6 6 -- ax + by +c = 0 here for line_2 = Line 1 0 (-8) --> a = 1, b = 0, c = -8 


-- compute line intersection
intersect:: Either String Geometry
intersect = intersection line_2 line_1

-- display line intersection output using 'line' (without quotes) command
line = 
  putStrLn $
  case intersect of
    Left x -> x
    Right x -> show x


------------------------------------------------------------------------------------------------------------------------------------

-- Plane intersection
--Please provide input in plane_1 and plane_2 for plane intersection use command 'plane' (without quotes) in console to display plane intersection output

plane_1:: Geometry
plane_1 = Plane 0 1 1 4 -- Ax + By + Cz + D = 0 here for plane_1 = Plane 0 2 3 6 --> A = 0, B = 2, C = 3, D = 6

plane_2:: Geometry
plane_2 = Plane 4 0 5 2 -- Ax + By + Cz + D = 0 here for plane_2 = Plane 0 2 3 6 --> A = 0, B = 2, C = 3, D = 6

-- compute plane intersection
plane_intersect:: Either String Geometry
plane_intersect = intersection plane_2 plane_1

-- display plane intersection output using 'plane' (without quotes) command
plane=
  putStrLn $
  case plane_intersect of
    Left x -> x
    Right x -> show x

-- if plane_1 = Plane 1 1 3 2  (x + y + 3*z + 2 = 0) and plane_2 = Plane 6 5 4 6 (6*x + 5*y + 4*z + 6 = 0)
-- Mathematically, the parametric line equation of intersection line of above planes would be --> (4,11,-6) + t*(11, -14, 1) where t is a real number. The line can also be mentioned as (4 + 11*t, -6 - 14*t, 0 + 1*t)
-- The output of plane intersection will look like --> ParamerticLine {point = Point3D {x = 4.0, y = -6.0, z = 0.0}, parameter = Point3D {x = 11.0, y = -14.0, z = 1.0}}
-- Hence the coordinates of line would be (4 + 11*t, -6 - 14*t, 0 + 1*t) where t is a real number. if we put t = 1 then (15, -20, 1) will lie on the intersection line of the planes and on both the planes.
-- Similarly we can put t with any real number. The code output is similar to mathematical output of the parametric notation of line.


