-- Vector operating library. 
-- 16/05/2020 Resbi 

module ExtraMath.Submodules.Vector where 

import Debug.Trace 

data Vector = Vector { 
     dimention :: Int, 
     component :: [Double] 
} deriving (Show) 

-- Do operation to each component between two vectors. 
-- "dOTECBTV" stand for it. 
-- dOTECBTV(func, a, b) = func(a, b) 
doOperationToEachComponentBetweenTwoVectors :: (Double -> Double -> Double) -> Vector -> Vector -> Vector 
doOperationToEachComponentBetweenTwoVectors func a b = if ( ( dimention a ) /= ( dimention b ) ) 
                                      then ( trace ( " [Error: Vector dimention error] \n\tmodule: Vector \n\tfunction: doOperationToEachComponentBetweenTwoVectors \n\tdimention doesn't match: " ++ ( show $ dimention a ) ++ " with " ++ ( show $ dimention b ) ) ( Vector 0 [] ) ) 
                                      else ( Vector ( dimention a ) [(func) ( ( component a ) !! index ) ( ( component b ) !! index ) | index <- [0..( ( dimention a ) - 1 )]] ) 

-- Do operation to each component in one vector. 
-- "dOTECIOV" stand for it. 
doOperationToEachComponentInOneVector :: ( Double -> Double ) -> Vector -> Vector 
doOperationToEachComponentInOneVector func a = Vector ( dimention a ) [func comp | comp <- ( component a )] 

-- Get the opposite vector. 
-- oppositeVec(a) := dOTECIOV(((-)0), a) 
oppositeVec :: Vector -> Vector 
oppositeVec a = doOperationToEachComponentInOneVector ((-)0) a 

-- Add two vectors. 
-- addVec(a, b) := dOTECBTV((+), a, b) 
addVec :: Vector -> Vector -> Vector 
addVec a b = doOperationToEachComponentBetweenTwoVectors (+) a b 

-- Subtract two vectors. 
-- subVec(a, b) := dOTECBTV((-), a, b) 
subVec :: Vector -> Vector -> Vector 
subVec a b = doOperationToEachComponentBetweenTwoVectors (-) a b 

-- Get the Dot Product of two vectors. 
-- dotproductVec(a, b) := norm^2(dOTECBTV((*), a, b)) 
dotproductVec :: Vector -> Vector -> Double 
dotproductVec a b = foldr (+) 0 ( component $ doOperationToEachComponentBetweenTwoVectors (*) a b ) 

-- Get the norm of vector. 
normVec :: Vector -> Double 
normVec a = sqrt $ foldr ((+).(**2)) 0 ( component a ) 

-- Get cos of the included angle between two vectors. 
-- getcosVec(a, b) = dotproductVec(a, b) / (normVec(a) * normVec(b)) 
getcosVec :: Vector -> Vector -> Double 
getcosVec a b = ( dotproductVec a b ) / ( ( normVec a ) * ( normVec b ) ) 

-- Get the included angle(radian) between two vectors. 
-- getangleVec(a, b) := acos(getcosVec(a, b)) 
--                    = <a, b> 
getangleVec :: Vector -> Vector -> Double 
getangleVec a b = acos $ getcosVec a b 

-- Judge if two vector are parallel. 
-- If it is, return True, else return False. 
-- ifparallelVec(a, b) := if ( abs(getcosVec(a, b)) == 1 ) 
--                        then True 
--                        else False 
-- Deleted at 17/05/2020 
--   reason: dosen't accurate enouth. 
--   E.g. [1.5, 2.3, 3.6] // [3.0 4.6 7.2] => False 
--ifparallelVec :: Vector -> Vector -> Bool 
--ifparallelVec a b = ( ( abs ( getcosVec a b ) ) == 1 ) 

-- Show a vector. 
-- Show it's components. 
showVec :: Vector -> String 
showVec a = show ( component a ) 

