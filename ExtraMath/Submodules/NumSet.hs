-- 18/5/2020 Resbi 
-- Numerical set library. 
-- A Numerical set are constructed by many combined numerical intervals. 

module ExtraMath.Submodules.NumSet where 

data NotatingBrackets = Closed 
                      | Open 
                      deriving (Show) 

data NumInterval = NumInterval { 
                                leftBracket  :: NotatingBrackets, 
                                rightBracket :: NotatingBrackets, 
                                leftEnd      :: Double, 
                                rightEnd     :: Double 
                               } 
                 | EmptyInterval 
                 deriving (Show) 

data NumSet = NumSet { 
                      intervals :: [NumInterval] 
                     } 
            | EmptySet 
            deriving (Show) 
