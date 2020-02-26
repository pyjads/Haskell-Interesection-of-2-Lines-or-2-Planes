module Geometry where

-- Point2D defined by x,y
-- Line defined by ax + by + c = 0 hence storing a,b,c as xCoeff, yCoeff and constant
-- Plane defined by Ax + By + Cz + D = 0 hence storing A, B, C, D as xCoeff, yCoeff, zCoeff and constant. The line output of plane intersection would be in Paramertic Line notation form.

data Geometry = Point2D {xCoord:: Double
                        ,yCoord:: Double} |
                Point3D {x:: Double
                        ,y:: Double
                        ,z:: Double}|
                Line  {xCoeff:: Double 
                      ,yCoeff:: Double
                       ,constant:: Double} |
                Plane {xCoeff:: Double,yCoeff:: Double, 
                       zCoeff::Double,
                       constant::Double} |
                ParamerticLine {point:: Geometry,
                        parameter:: Geometry} deriving (Show)
  

intersection:: Geometry -> Geometry -> Either String Geometry
intersection (Line a1 b1 c1) (Line a2 b2 c2)
                      | a1 == 0 && b1 == 0 || a2 == 0 && b2 ==0 = error "Incorrect line. Please check for atleast one of the x or y coefficient of each line should be non zero"
                      | isInfinite m1 && isInfinite m2 && ((-c2)*a1) == ((-c1)*a2) || (-a1)*b2 == (-a2)*b1 && (-c1)*b2 == (-c2)*b1 && isInfinite m1 /= True && isInfinite m2 /= True  = Left ("Given two lines are identical. Hence infinite solutions exist")
                      | cond1 == 0 = Left  ("Given Two Lines are Parallel")
                      | cond1 /= 0 = Right (Point2D x y)
                      | otherwise = Left  ("Please check the implementation for missing test case")
                      where { m1 = (-a1)/b1 ;        -- slope of first line
                              m2 = (-a2)/b2;         -- slope of second line
                              intercept1 = (-c1)/b1; -- intercept with y-axis for first line
                              intercept2 = (-c2)/b2; -- intercept with y-axis for second line
                              cond1 = (a1*b2) - (a2*b1); 
                              cond2 = (b1*c2)-(b2*c1);
                              cond3 = (c1*a2)-(c2*a1);
                              x = cond2 / cond1;     -- x = (b1*c2 - b2*c1)/(a1*b2 - a2*b1) 
                              y = cond3/cond1 }      -- y = (c1*a2 - c2*a1)/ (a1*b2 - a2*b1)


intersection (Plane a1 b1 c1 d1) (Plane a2 b2 c2 d2)
                      | (a1 == 0 && b1 ==0 && c1 ==0) || (a2==0 && b2 ==0 && c2 ==0) = error "The Given Input is not a valid Plane"
                      | dot_product*dot_product == normal1_mod*normal2_mod  && (isNaN (d1/d2) || d1*a2 == a1*d2 && isInfinite (a1/a2) == False && isNaN (a1/a2) == False || d1*b2 == b1*d2 && isInfinite (b1/b2) == False && isNaN (b1/b2) == False || d1*c2 == c1*d2 && isInfinite (c1/c2) == False && isNaN (c1/c2) == False)  = Left $"Given Planes are identical"
                      | dot_product*dot_product == normal1_mod*normal2_mod  = Left $"Given Planes are Parallel"
                      | n1_n2_z /= 0 = Right (ParamerticLine (Point3D t1_z t2_z 0) (Point3D n1_n2_x n1_n2_y n1_n2_z)) -- if intersection line passes through z axis
                      | n1_n2_y /= 0 = Right (ParamerticLine (Point3D t1_y 0 t2_y) (Point3D n1_n2_x n1_n2_y n1_n2_z)) -- if intersection line passes through y axis
                      | n1_n2_x /= 0 = Right (ParamerticLine (Point3D 0 t1_x t2_x) (Point3D n1_n2_x n1_n2_y n1_n2_z)) -- if intersection line passes through x axis
                      | otherwise = Left ("Please check the implementation for missing test case")
                       where {normal1_mod = (a1*a1) + (b1*b1) + (c1*c1); -- modulus of normal Vector of Plane1
                              normal2_mod = (a2*a2) +(b2*b2)+(c2*c2);    -- modulus of normal Vector of Plane2
                              dot_product = (a1*a2 + b1*b2 +c1*c2);      -- dot product of normal vectors of Plane1 and Plane2
                              n1_n2_x = (b1*c2)-(b2*c1);                 -- condition to check if intersection line of two planes passes through x axis, In this case (b1*c2)-(b2*c1) !=0
                              n1_n2_y =(a2*c1)-(a1*c2);                  -- condition to check if intersection line of two planes passes through y axis, In this case (a2*c1)-(a1*c2) !=0
                              n1_n2_z = (a1*b2)-(a2*b1);                 -- condition to check if intersection line of two planes passes through z axis, In this case (a1*b2)-(a2*b1) ! = 0
                              Right (Point2D t1_x t2_x) = intersection (Line b1 c1 d1) (Line b2 c2 d2); -- computing intersection for two lines b1y + c1z + d1 = 0 and b2y+c2z + d2 = 0
                              Right (Point2D t1_y t2_y) = intersection (Line a1 c1 d1) (Line a2 c2 d2); -- computing intersection for two lines a1x + c1z + d1 = 0 and a2x + c2z + d2 = 0
                              Right (Point2D t1_z t2_z) = intersection (Line a1 b1 d1) (Line a2 b2 d2);} -- computing intersection for two lines a1x + b1y + d1 = 0 and a2x + b2y + d2 = 0                 
