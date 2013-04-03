module Utils.Colour where

hsl2rgb :: Double -> Double -> Double -> (Integer, Integer, Integer)
hsl2rgb h s l = (r, g, b)
  where c = (1 - abs (2 * l - 1)) * s
        h' = h / 60.0
        x = c * (1 - abs ((h' `mod'` 2) - 1))
        (r1, g1, b1) | h' >= 0 && h' < 1 = (c, x, 0)
                     | h' >= 1 && h' < 2 = (x, c, 0)
                     | h' >= 2 && h' < 3 = (0, c, x)
                     | h' >= 3 && h' < 4 = (0, x, c)
                     | h' >= 4 && h' < 5 = (x, 0, c)
                     | otherwise       = (c, 0, x)
        m = l - (c / 2)
        r = round $ (r1 + m) * 255
        g = round $ (g1 + m) * 255
        b = round $ (b1 + m) * 255
        mod' :: Double -> Double -> Double
        d `mod'` e = let d' = floor d
                         e' = round e
                         d'' = d - fromInteger d'
                         res = d' `mod` e'
                     in fromInteger res + d''
