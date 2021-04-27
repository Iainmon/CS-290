

import Text.Printf

roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr = printf "%0.*f"


bounceFrames :: Int -> String
bounceFrames n = unlines $ map (\x -> ".major h2 span:nth-child("++show x++") { animation-delay: " ++ (roundToStr 3 (((fromIntegral x)/(fromIntegral n)) :: Double)) ++ "s; }") [2..n]

chars :: String -> [String]
chars = map (:[])

spanWrap :: String -> String
spanWrap s = "<span>"++s++"</span>"

bounceformat :: String -> String
bounceformat = unlines . map spanWrap . chars

compileBounceHTML = putStrLn . bounceformat