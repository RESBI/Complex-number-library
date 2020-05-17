-- ComplexNumber lib testing demo : simple calculating. 
-- 15/5/2020 Resbi 
module Main where 

import ExtraMath.ExtraMath 

main :: IO () 
main = do let a = double2CN 2 3 
          let b = double2CN 8 7 
          let c = double2ExpCN 1.2 (e*pi) 
          let d = double2ExpCN 10 (pi/2) 
          let u = Vector 3 [1.5, 2.3, 3.6] 
          let v = Vector 3 [3.0, 4.6, 7.2] 
          let m = Vector 3 [2.9, 3.1, 4.5] 
          let n = Vector 4 [3.4, 6.2, 6.6, 0.4] 
          putStrLn "[0] Complex number operating: "
          putStrLn $ (++) ( "conjugate( " ++ ( showCN a ) ++ " ) = " ) $ showCN $ conjugateCN a 
          putStrLn $ (++) ( "norm( " ++ ( showCN a ) ++ " ) = " ) $ show $ normCN a 
          putStrLn $ (++) ( "( " ++ ( showCN a ) ++ " ) + ( " ++ ( showCN b ) ++ " ) = " ) $ showCN $ addCN a b 
          putStrLn $ (++) ( "( " ++ ( showCN a ) ++ " ) - ( " ++ ( showCN b ) ++ " ) = " ) $ showCN $ subCN a b 
          putStrLn $ (++) ( "( " ++ ( showCN a ) ++ " ) * ( " ++ ( showCN b ) ++ " ) = " ) $ showCN $ mulCN a b 
          putStrLn $ (++) ( "( " ++ ( showCN a ) ++ " ) / ( " ++ ( showCN b ) ++ " ) = " ) $ showCN $ divCN a b 
          putStrLn $ (++) ( ( showExpCN c) ++ " => " ) $ showCN $ exp2Normal c 
          putStrLn $ (++) ( ( showCN a ) ++ " => " ) $ showExpCN $ normal2Exp a 
          putStrLn $ (++) ( "( " ++ ( showExpCN c) ++ " ) + ( " ++ ( showExpCN d ) ++ " ) = " ) $ showExpCN $ addExpCN c d 
          putStrLn $ (++) ( "( " ++ ( showExpCN c) ++ " ) - ( " ++ ( showExpCN d ) ++ " ) = " ) $ showExpCN $ subExpCN c d 
          putStrLn $ (++) ( "( " ++ ( showExpCN c) ++ " ) * ( " ++ ( showExpCN d ) ++ " ) = " ) $ showExpCN $ mulExpCN c d 
          putStrLn $ (++) ( "( " ++ ( showExpCN c) ++ " ) / ( " ++ ( showExpCN d ) ++ " ) = " ) $ showExpCN $ divExpCN c d 
          putStrLn "\n[1] Vector operating: " 
          putStrLn $ (++) "u = " $ showVec u 
          putStrLn $ (++) "v = " $ showVec v 
          putStrLn $ (++) "m = " $ showVec m 
          putStrLn $ (++) "n = " $ showVec n 
          putStrLn "Operating if dimention match: " 
          putStrLn $ (++) "n + v  = " $ showVec $ addVec u v 
          putStrLn $ (++) "u - m  = " $ showVec $ subVec u m 
          putStrLn $ (++) "v * m  = " $ show $ dotproductVec u v 
          putStrLn $ (++) "<u, m> = " $ show $ getangleVec u m 
          putStrLn $ (++) "  |v|  = " $ show $ normVec v 
          putStrLn "Operating if dimention doesn't match: " 
          putStrLn $ (++) "m + n  = " $ showVec $ addVec m n 

