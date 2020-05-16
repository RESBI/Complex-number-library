-- ExtraMath library. 
-- 16/05/2020 Resbi 
-- Actually, I just want to write a complex number lib, 
-- But I found that I want to write something else but also maths, 
-- So I combined the ComplexNumber lib to ExtraMathLib. 
-- OHMYCODELOOKSLIKEASTRINGOFNOODLE
--
-- todos: 
--    15/05/2020: 
--      [ 16/05/2020 ] add ComplexNumber submodule. 
--      [ 16/05/2020 ] add exp-type complex number and operating functions for ComplexNumber submodule. 
--    16/05/2020: 
--      [ 16/05/2020 ] Vector lib. 
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
--                    cn2Vec 

module ExtraMath ( 
  module ComplexNumber, 
  module Vector 
) where 

import ComplexNumber 
import Vector 

--vec2CN :: Vector -> ComplexNumber 

-- transfor a normal complex number to a 2D vector. 
cn2Vec :: ComplexNumber -> Vector 
cn2Vec a = Vector 2 [real a, imag a] 
