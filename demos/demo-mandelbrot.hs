-- ComplexNumber lib testing demo: mandelbrot. 
-- 16/5/2020 Resbi 
module Main where 

import ExtraMath 

main :: IO () 
--                                          Escaping radius                                               Wdith/2  scale                       Height/2  scale                                      Width                               Hieght        Width
--                                                ¡ý                                                          ¡ý       ¡ý                             ¡ý       ¡ý                                          ¡ý                                   ¡ý             ¡ý
main = putStrLn $ foldl (++) "" [( if ( getEscape 21 ( double2CN 0 0 ) $ double2CN ( (/) ( fromInteger ( x - 20 ) ) 10 ) ( (/) ( fromInteger ( y - 20 ) ) 10 ) ) then "  " else "##" ) ++ ( if ( x == 40 ) then "\n" else "" ) | y <- [0..40], x <- [0..40]] 
       where 
         getEscape 0    _ _     = False 
         getEscape temp c point = if ( ( normCN c ) >= 2 ) 
                                  then True 
                                  else ( getEscape ( temp - 1 ) ( addCN ( mulCN c c ) point ) point ) 

