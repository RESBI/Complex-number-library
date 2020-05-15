-- 13/5/2020 Resbi 
-- complex number library 
-- OHMYCODELOOKSLIKEASTRINGOFNOODLE
-- todo: 
--    add exp-type complex number and operating functions for it. 
-- 15/5/2020 
--    add: 
--        functions: 
--            double2CN, 
--            operateComplexNumberByPart, 
--            normCN, 
--            addCN, 
--            subCN, 
--            mulCN, 
--            divCN, 
--            conjugateCN, 
--            showCN 

module ComplexNumber where 

-- Defining complex number data type. 
data ComplexNumber = ComplexNumber {
                     real :: Double, 
                     imag :: Double
} deriving (Show) 

-- Showing a complex number. 
showCN :: ComplexNumber -> String 
showCN a =  (++"i") $ (++) ( show ( real a ) ) ( if ( ( imag a ) >= 0 ) then ( "+" ++ ( show ( imag a ) ) ) else ( show ( imag a ) ) ) 

-- Generate complex number by two double-type numbers. 
-- double2CN(a, b) = a + b*i
double2CN :: Double -> Double -> ComplexNumber
double2CN a b = ComplexNumber a b 

-- "oCNBP" stand for the function "operateComplexNumberByPart" 
-- oCNBP(func1, func2, a, b) := func1(Re(a), Re(b)) + func2(Im(a), Im(b))*i 
operateComplexNumberByPart :: ( Double -> Double -> Double ) -> ( Double -> Double -> Double ) -> ComplexNumber -> ComplexNumber -> ComplexNumber 
operateComplexNumberByPart operateReal operateImag a b = double2CN ( (operateReal) ( real a ) ( real b ) ) ( (operateImag) ( imag a ) ( imag b ) ) 

-- Get norm of a complex number. 
-- norm(a) = ( |a| =) sqrt( Re^2(a) + Im^2(a) ) 
normCN :: ComplexNumber -> Double 
normCN a = sqrt $ (+) ( (^2) $ real a ) ( (^2) $ imag a ) 

-- Add two complex numbers up. 
-- add(a, b) := oCNBP((+), (+), a, b) 
--            = a + b 
addCN :: ComplexNumber -> ComplexNumber -> ComplexNumber 
addCN a b = operateComplexNumberByPart (+) (+) a b 

-- Subtract two complex numbers. 
-- sub(a, b) := oCNBP((-), (-), a, b) 
--            = a - b 
subCN :: ComplexNumber -> ComplexNumber -> ComplexNumber 
subCN a b = operateComplexNumberByPart (-) (-) a b 

-- Multiply two complex numbers up. 
-- mul(a, b) = a * b 
--           = Re(a)*Re(b) - Im(a)*Im(b) + ( Re(a)*Im(b) + Re(b)*Im(a) )*i 
mulCN :: ComplexNumber -> ComplexNumber -> ComplexNumber 
mulCN a b = double2CN ( ( ( real a ) * ( real b ) ) - ( ( imag a ) * ( imag b ) ) ) ( ( ( real a ) * ( imag b ) ) + ( ( real b ) * ( imag a ) ) ) 

-- Divide two complex numbers. 
-- div(a, b) = a / b 
--           = ( a * ( conjugate b ) ) / ( b * ( conjugate b ) ) 
--           = (Re(a)*Re(b) + Im(a)*Im(b))/(norm^2(b)) + ( Re(b)*Im(a) - Re(a)*Im(b) )*i/(norm^2(b))
divCN :: ComplexNumber -> ComplexNumber -> ComplexNumber 
divCN a b = double2CN ( ( ( ( real a ) * ( real b ) ) + ( ( imag a ) * ( imag b ) ) ) / ( (+) ( (^2) $ real b ) ( (^2) $ imag b ) ) ) ( ( ( ( real b ) * ( imag a ) ) - ( ( real a ) * ( imag b ) ) ) / ( (+) ( (^2) $ real b ) ( (^2) $ imag b ) ) ) 

-- Get the conjugate complex number. 
-- conjugate(a) = Re(a) - Im(a) 
--              = oCNBP((+), (-), (0+0i), a) 
conjugateCN :: ComplexNumber -> ComplexNumber 
conjugateCN a = double2CN ( real a ) ( - ( imag a ) ) 

