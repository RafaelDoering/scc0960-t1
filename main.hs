stringToInteger :: String -> Integer
stringToInteger str = read str :: Integer

integerToString :: Integer -> String
integerToString integer = show integer

joinStrings :: String -> String -> String
joinStrings str1 str2 = str1 ++ str2

formatPoints :: Integer -> String
formatPoints points = if (points == 10) then "X"
                      else integerToString points

formatNormalFrame :: Integer -> Integer -> String
formatNormalFrame firstThrow secondThrow = joinStrings (formatPoints firstThrow) $ joinStrings " " $ joinStrings (formatPoints secondThrow) " | "

formatSpareFrame :: Integer -> String
formatSpareFrame firstThrow = joinStrings (formatPoints firstThrow) " / | "

formatStrikeFrame :: () -> String
formatStrikeFrame () = "X _ | "

formatLastFrame :: Integer -> Integer -> Integer -> String
formatLastFrame firstThrow secondThrow thirdThrow = if ((firstThrow + secondThrow) == 10) then joinStrings (formatPoints firstThrow) $ joinStrings " / " $ joinStrings (formatPoints thirdThrow) " | "
else joinStrings (formatPoints firstThrow) $ joinStrings " " $ joinStrings (formatPoints secondThrow) $ joinStrings " " $ joinStrings (formatPoints thirdThrow) " | "

formatFrames :: [Integer] -> String
formatFrames ([]) = ""
formatFrames (x : []) = formatNormalFrame x 0
formatFrames (x : y : []) = formatNormalFrame x y
formatFrames (x : y : z : []) = formatLastFrame x y z
formatFrames (x : y : xs) = if (x == 10) then joinStrings (formatStrikeFrame ()) (formatFrames (y : xs))
else if ((x + y) == 10) then joinStrings (formatSpareFrame x) (formatFrames xs)
else joinStrings (formatNormalFrame x y) (formatFrames xs)

getScore :: [Integer] -> String
getScore numbersOfPins = formatFrames numbersOfPins

getPoints :: String -> [Integer]
getPoints line = map stringToInteger $ words line

getTotalScore :: [Integer] -> Integer
getTotalScore points = foldr (+) 0 points

printIntegerList integerList = putStrLn $ show integerList

getScoreWithTotal :: String -> String
getScoreWithTotal line = joinStrings (getScore $ getPoints line) (integerToString $ getTotalScore $ getPoints line)

printScore line = putStrLn $ getScoreWithTotal line

main = do
  line <- getLine
  printScore line