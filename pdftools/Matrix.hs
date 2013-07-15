

module Matrix where
import Data.List
import Numeric

data Matrix = Matrix !Double !Double !Double !Double !Double !Double
     deriving (Eq,Ord)

identity :: Matrix
identity  = Matrix 1 0 0 1 0 0

scaling, offset :: Double -> Double -> Matrix
scaling x y = Matrix x 0 0 y 0 0
offset x y = Matrix 1 0 0 1 x y

rotation :: Double -> Matrix
rotation r = Matrix a b (-b) a 0 0
    where a = cos r
          b = sin r

matrix :: Double -> Double -> Double -> Double -> Double -> Double -> Matrix
matrix a b c d e f = Matrix a b c d e f

mul :: Matrix -> Matrix -> Matrix
mul (Matrix l11 l12 l21 l22 lx ly) (Matrix r11 r12 r21 r22 rx ry) =
         Matrix (l11*r11 + l12*r21)
                (l11*r12 + l12*r22)
                (l21*r11 + l22*r21)
                (l21*r12 + l22*r22)
                (lx*r11 + ly*r21 + rx)
                (lx*r12 + ly*r22 + ry)

instance Show Matrix where
    show (Matrix a b c d e f) = "[" ++ (concat . intersperse " " . map (\x -> showFFloat Nothing x "") $ [a,b,c,d,e,f]) ++ "]"

