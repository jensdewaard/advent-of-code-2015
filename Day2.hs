module Day2 where

data Present = Present Int Int Int

wrappingPaper :: Present -> Int
wrappingPaper (Present l h w) = 2 * l * w + 2 * w * h + 2 * h * l + min (l * w) (min (w * h) (h * l))

ribbon :: Present -> Int
ribbon (Present l h w) = l*h*w + 2*(l+h+w) - 2*max w (max h l)

parsePresent :: String -> Present
parsePresent s =
  let repl 'x' = ' '
      repl c = c
   in let (x : y : z : ns) = map (read :: String -> Int) $ words $ map repl s in Present x y z