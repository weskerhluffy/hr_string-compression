import Control.Monad
import Data.List (concatMap, group)

anadir:: Char -> Int -> String
anadir '\0' _ = ""
anadir c 0 = ""
anadir c 1 = [c]
anadir c n = [c]++(show n)

compress':: String ->Char->Int -> String -> String
compress' "" c n res = res++(anadir c n)
compress' (pc:px) c n res
        | c /= pc = compress' px pc 1 (res++(anadir c n))
        | otherwise = compress' px pc (n+1) res

compress:: String -> String
compress p = ((concatMap format).group) p
        where
                format [c] = [c]
                format p' = (head p'):(show.length) p'

main :: IO ()
main = do
  p<- getLine
  putStrLn $ compress p
