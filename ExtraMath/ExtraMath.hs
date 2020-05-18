-- ExtraMath library. 
-- 16/05/2020 Resbi 
-- Actually, I just want to write a complex number lib, 
-- But I found that I want to write something else but also maths, 
-- So I combined the ComplexNumber lib with ExtraMathLib. 
-- OHMYCODELOOKSLIKEASTRINGOFNOODLE
--
-- todos: 
--    15/05/2020: 
--      [ 16/05/2020 ] add ComplexNumber submodule. 
--      [ 16/05/2020 ] add exp-type complex number and operating functions for ComplexNumber submodule. 
--    16/05/2020: 
--      [ 16/05/2020 ] Vector lib. 
--      [            ] LinearAlgebra lib. 
--    18/05/2020: 
--      [            ] Numerical set lib. 
--      [            ] Geometry lib. 
--
-- Changes: 
--    15/05/2020 
--        add: 
--            ComplexNumber: 
--                functions: 
--                    double2CN, 
--                    operateComplexNumberByPart, 
--                    normCN, 
--                    addCN, 
--                    subCN, 
--                    mulCN, 
--                    divCN, 
--                    conjugateCN, 
--                    showCN 
--    16/05/2020 
--        change: 
--            restructured the whole library. 
--        add: 
--            ComplexNumber: 
--                functions: 
--                    double2ExpCN, 
--                    operateExponentialComplexNumberByPart, 
--                    addExpCN, 
--                    subExpCN, 
--                    mulExpCN, 
--                    divExpCN, 
--                    showExpCN, 
--                    getRadian, 
--                    getRadianCN, 
--                    normal2Exp, 
--                    exp2Normal, 
--                    expCN 
--            Vector: 
--                    doOperationToEachComponentBetweenTwoVectors, 
--                    doOperationToEachComponentInOneVector, 
--                    addVec, 
--                    subVec, 
--                    normVec, 
--                    dotproductVec, 
--                    oppositeVec, 
--                    getcosVec, 
--                    ifparallelVec 
--            ExtraMath: 
--                    cn2Vec, 
--                    vec2CN 
--    17/05/2020 
--        change: 
--            restructured the whole library. 
--        add: 
--            Vector: 
--                    showVec, 
--                    getangleVec 
--        delete: 
--            Vector: 
--                    ifparallelVec 

module ExtraMath.ExtraMath ( 
                            module ExtraMath.Submodules.ComplexNumber, 
                            module ExtraMath.Submodules.Vector --, 
                            --module ExtraMath.Submodules.NumSet, 
                            --module ExtraMath.Submodules.Geometry, 
                            --module ExtraMath.Submodules.FracNum 
                           ) where 

import Debug.Trace 

import ExtraMath.Submodules.ComplexNumber 
import ExtraMath.Submodules.Vector 

-- Transfor a 2D vector to a normal complex number. 
vec2CN :: Vector -> ComplexNumber 
vec2CN a = if ( ( dimention a ) /= 2 ) 
           then ( trace ( " [Error: Vector dimention error] \n\tmodule: ExtraMath \n\tfunctuon: vec2CN \n\tdimention dosen't match: " ++ ( show $ dimention a ) ++ " with 2. " ) ( double2CN 0 0 ) ) 
           else ( ComplexNumber ( ( component a ) !! 0 ) ( ( component a ) !! 1 ) ) 

-- Transfor a normal complex number to a 2D vector. 
cn2Vec :: ComplexNumber -> Vector 
cn2Vec a = Vector 2 [real a, imag a] 

