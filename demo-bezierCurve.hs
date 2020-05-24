-- 23/05/2020 Resbi 
-- A bezier curve drawing program. 

module Main where 

import Debug.Trace 
import ExtraMath.ExtraMath 

-- Get a bezier curve. 
bezierCurve :: [Vector] -> Int -> Int -> [Vector] 
bezierCurve points n howmany = [bezierPoint points n $ (/) ( fromIntegral t ) $ fromIntegral $ (subtract 1) howmany | t <- [0..(howmany - 1)]] 

-- Get a bezier point in bigger then 1 dimension. 
bezierPoint :: [Vector] -> Int -> Double -> Vector 
bezierPoint []     n _ = trace ( (++) "No enough points! -" $ show n ) $ Vector 0 []
bezierPoint points 0 _ = head points 
bezierPoint points n t = bezierPoint [addVec ( (!!) points k ) $ doOperationToEachComponentInOneVector (*t) $ subVec ( (!!) points $ (+1) k ) ( (!!) points k ) | k <- [0..((subtract 1) n)]] ( (subtract 1) n ) t 

generateBoard :: Int -> Int -> Double -> [Vector] -> [Vector] -> String 
generateBoard height width scale points curve = foldr (++) "" $ addEnterChar width height $ coreFunc width height "@@" ( transferVec scale points ) $ coreFunc width height "##" ( transferVec scale curve ) [".." | y <- [0..height], x <- [0..width]] 
                                                where 
                                                  transferVec scale vecs               = [[round a | a <- ( component $ doOperationToEachComponentInOneVector (*scale) vec )] | vec <- vecs] 
                                                  coreFunc _ _ _    []           board = board 
                                                  coreFunc w h char (now:points) board = coreFunc w h char [p | p <- points, p /= now] [if ( (==) [x, y] now ) then ( char ) else ( (!!) board $ (+) x $ (*((+1)w)) y ) | y <- [0..h], x <- [0..w]] 
                                                  addEnterChar w h string              = [(if ( (==0) $ mod ((+1) x) ((+1) w) ) then (++"\n") else (id)) $ (!!) string $ (+x) $ (*((+1)w)) y | y <- [0..h], x <- [0..w]] 

getPoints :: Int -> Int -> [Vector] -> IO () 
getPoints order 0         points = trace "Calculating..." $ trace ( generateBoard 39 39 4 points $ bezierCurve ( reverse points ) order 100 ) ( getLine >>= \_ -> putStrLn "Cu next time!" ) 
getPoints order countDown points = getLine >>= \args -> getPoints order ( (subtract 1) countDown ) $ (:) ( makeVector [read x::Double | x <- ( words args )] ) points 
                                   where 
                                     makeVector point = Vector ( length point ) point 

main :: IO () 
main = trace "This is a bezil curve drawing demo for ExtraMath lib. \nYou will be asked to input two things: order and some 2-D points. \nOrder is the order of the bezil curve, and the points are for generating the curve. \nE.g. \n2 \n0 0 \n2 7 \n9 1 \n( Notice, the amount of points is order + 1, order > 0 and the format of points are \"x y\" ) \nAnd then the program will draw the curve. \nNow, please input order: " $ readLn >>= \order -> trace ( (++ " 2-D points, which's x and y location must in [0, 10] : ") $ (++) "Please input " $ show $ (+1) order ) getPoints order ( (+1) order ) [] 
