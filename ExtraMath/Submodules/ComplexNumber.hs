-- 13/5/2020 Resbi 
-- complex number submodule

module ExtraMath.Submodules.ComplexNumber where 

-- Constants here. 
e :: Double 
e = 2.71828182845904523536028747135266249775724709369995 

-- Defining complex number data type. 
data ComplexNumber = ComplexNumber {
     real :: Double, 
     imag :: Double
} deriving (Show) 

data ExponentialComplexNumber = ExponentialComplexNumber {
     radius :: Double, 
     radian :: Double -- Actually, I want to use "degree" here instead of "radian". 
} deriving (Show) 

-- Get radian of vector (a, b). 
-- getRadian(a, b) = if b >= 0 
--                   then ( atan( b/a ) ) 
--                   else if b >= 0 
--                        then ( pi + atan( b/a ) ) 
--                        else ( 2*pi - atan( b/a ) )
getRadian :: Double -> Double -> Double 
getRadian a b = ( if ( b < 0 ) then ( if ( a >= 0 ) then ( (+pi) ) else ( ((-)(2*pi)) ) ) else ( (id) ) ) $ atan ( b / a ) 

-- Normal complex number functions: 

-- Get radian of complex number. 
-- getRadianCN(a) := getRadian(Re(a), Im(a))
getRadianCN :: ComplexNumber -> Double 
getRadianCN a = getRadian ( real a ) ( imag a )

-- Showing a complex number. 
showCN :: ComplexNumber -> String 
showCN a =  (++" * i") $ (++) ( show ( real a ) ) ( if ( ( imag a ) >= 0 ) then ( " + " ++ ( show ( imag a ) ) ) else ( show ( imag a ) ) ) 

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
-- conjugate(a) = Re(a) - Im(a)*i 
--              = oCNBP((+), (-), (0+0i), a) 
conjugateCN :: ComplexNumber -> ComplexNumber 
conjugateCN a = double2CN ( real a ) ( - ( imag a ) ) 

-- (a+bi)ᶜ⁺ᵈⁱ 
-- let ŗ⋅eᶿⁱ = a+bi 
--         r = norm(a+bi) 
-- r⋅eᶿⁱ=eˡⁿ⁽ʳ⁾⁺ᶿⁱ 
-- so 
-- expCN((a+bi), (c+di)) = (a+bi)ᶜ⁺ᵈⁱ = e⁽ˡⁿ⁽ʳ⁾⁺ᶿⁱ⁾⁽ᶜ⁺ᵈⁱ⁾ 
--                       = eᶜˡⁿ⁽ʳ⁾⁺ⁱᵈˡⁿ⁽ʳ⁾⁺ᶜᶿⁱ⁻ᵈᶿ 
--                       = e⁽ᶜˡⁿ⁽ʳ⁾⁻ᵈᶿ⁾⁺⁽ᵈˡⁿ⁽ʳ⁾⁺ᶜᶿ⁾ⁱ 
--                       = eᶜˡⁿ⁽ʳ⁾⁻ᵈᶿ⋅(cos(d⋅ln(r)+c⋅θ)+i⋅sin(d⋅ln(r)+c⋅θ)) 
--                       = (rᶜ/eᵈᶿ)⋅(cos(d⋅ln(r)+c⋅θ)+i⋅sin(d⋅ln(r)+c⋅θ)) 
expCN :: ComplexNumber -> ComplexNumber -> ComplexNumber 
expCN a b = (\a b c d r theta -> double2CN ( ( ( r ** c ) / ( e ** ( d * theta ) ) ) * ( cos ( ( d * ( log r ) ) + ( c * theta ) ) ) ) ( ( ( r ** c ) / ( e ** ( d * theta ) ) ) * ( sin ( ( d * ( log r ) ) + ( c * theta ) ) ) )) ( real a ) (imag a ) ( real b ) ( imag b ) ( normCN a ) ( getRadianCN a ) 

-- Exponential complex number functions: 

-- Show a exponential complex number. 
showExpCN :: ExponentialComplexNumber -> String 
showExpCN a = ( show ( radius a ) ) ++ " * e^( " ++ ( show ( radian a ) ) ++ " * i )" 

-- double2CNExp(a, b) = a⋅eᵇⁱ 
double2ExpCN :: Double -> Double -> ExponentialComplexNumber 
double2ExpCN a b = ExponentialComplexNumber a b 

-- Transforing normal complex number to exponential complex number. 
-- norm2Exp(a) = norm(a) * e^(getRadianCN(a)*i) 
normal2Exp :: ComplexNumber -> ExponentialComplexNumber 
normal2Exp a = double2ExpCN ( normCN a ) ( getRadianCN a ) 

-- Transforing exponential complex number to normal complex number. 
-- exp2Normal(a) = Radius(a) * (cos(Radian(a)) + sin(Radian(a))*i) 
exp2Normal :: ExponentialComplexNumber -> ComplexNumber 
exp2Normal a = double2CN ( ( radius a ) * ( cos ( radian a ) ) ) ( ( radius a ) * ( sin ( radian a ) ) ) 

-- "oECNBP" stand for the function "operateExponentialComplexNumberByPart" 
-- oECNBP(func1, func2, a, b) := func1(Radius(a), Radius(b)) * e^(func2(Radian(a), Radian(b))*i) 
operateExponentialComplexNumberByPart :: ( Double -> Double -> Double ) -> ( Double -> Double -> Double ) -> ExponentialComplexNumber -> ExponentialComplexNumber -> ExponentialComplexNumber 
operateExponentialComplexNumberByPart operateRadius operateRadian a b = double2ExpCN ( (operateRadius) ( radius a ) ( radius b ) ) ( (operateRadian) ( radian a ) ( radian b ) ) 

-- Add two exponential complex numbers. 
-- addExpCN(a, b) := normal2Exp(addCN(exp2Normal(a), exp2Normal(b))) 
addExpCN :: ExponentialComplexNumber -> ExponentialComplexNumber -> ExponentialComplexNumber 
addExpCN a b = normal2Exp $ addCN ( exp2Normal a ) ( exp2Normal b ) 

-- Subtract two exponential complex numbers. 
-- subExpCN(a, b) := normal2Exp(subCN(exp2Normal(a), exp2Normal(b))) 
subExpCN :: ExponentialComplexNumber -> ExponentialComplexNumber -> ExponentialComplexNumber 
subExpCN a b = normal2Exp $ subCN ( exp2Normal a ) ( exp2Normal b ) 

-- Multiply two exponential complex numbers. 
-- mulExpCN(a, b) := oECNBP((*), (+), a, b) 
mulExpCN :: ExponentialComplexNumber -> ExponentialComplexNumber -> ExponentialComplexNumber 
mulExpCN a b = operateExponentialComplexNumberByPart (*) (+) a b 

-- Divide two exponential complex numbers. 
-- divExpCN(a, b) := oECNBP((/), (-), a, b) 
divExpCN :: ExponentialComplexNumber -> ExponentialComplexNumber -> ExponentialComplexNumber 
divExpCN a b = operateExponentialComplexNumberByPart (/) (-) a b 
