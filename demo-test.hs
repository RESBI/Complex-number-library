-- ComplexNumber lib testing demo : simple calculating. 
-- 15/5/2020 Resbi 
module ComplexNumberTestOne where 

import ComplexNumber 

main :: IO () 
main = do let a = double2CN 2 3 
          let b = double2CN 8 7 
          putStrLn $ (++) ( "conjugate( " ++ ( showCN a ) ++ " ) = " ) $ showCN $ conjugateCN a 
          putStrLn $ (++) ( "norm( " ++ ( showCN a ) ++ " ) = " ) $ show $ normCN a 
          putStrLn $ (++) ( ( showCN a ) ++ " + " ++ ( showCN b ) ++ " = " ) $ showCN $ addCN a b 
          putStrLn $ (++) ( ( showCN a ) ++ " - " ++ ( showCN b ) ++ " = " ) $ showCN $ subCN a b 
          putStrLn $ (++) ( ( showCN a ) ++ " * " ++ ( showCN b ) ++ " = " ) $ showCN $ mulCN a b 
          putStrLn $ (++) ( ( showCN a ) ++ " / " ++ ( showCN b ) ++ " = " ) $ showCN $ divCN a b 

