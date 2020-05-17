-- ComplexNumber lib testing demo: mandelbrot. 
-- 16/5/2020 Resbi 
module Main where 

import ExtraMath.ExtraMath 

genMandelbrot :: Integer -> Integer -> Integer -> Integer -> Double -> Double -> IO () 
--                                                                    Escaping radius                                                       Wdith/2                  scale x's offset                                Height/2                 scale y's offset                                      Width                              Hieght       Width
--                                                                           ¡ý                                                                 ¡ý                       ¡ý       ¡ý                                        ¡ý                       ¡ý       ¡ý                                             ¡ý                                  ¡ý            ¡ý
genMandelbrot w h s r offx offy = putStrLn $ foldl (++) "" [( if ( getEscape r ( double2CN 0 0 ) $ double2CN ( ( (/) ( fromInteger ( x - ( div w 2 ) ) ) ( fromInteger s ) ) + offx ) ( ( (/) ( fromInteger ( y - ( div h 2 ) ) ) ( fromInteger s ) ) + offy ) ) then "  " else "##" ) ++ ( if ( x == w ) then "\n" else "" ) | y <- [0..h], x <- [0..w]] 
                                  where 
                                    getEscape 0    _ _     = False 
                                    getEscape temp c point = if ( ( normCN c ) > 2 ) 
                                                             then True 
                                                             else ( getEscape ( temp - 1 ) ( addCN ( mulCN c c ) point ) point ) 

main :: IO () 
main = genMandelbrot 40 40 100000000 10000 (0.250900551066087389712564928) (0.000040912003949449095048797)
