-- ComplexNumber lib testing demo : simple calculating. 
-- 15/5/2020 Resbi 
module Main where 

import ExtraMath 

main :: IO () 
main = do let a = double2CN 2 3 
          let b = double2CN 8 7 
          let c = double2ExpCN 1.2 (e*pi) 
          let d = double2ExpCN 10 (pi/2) 
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

