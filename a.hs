import Control.Monad

compress':: String ->Char->Int -> String -> String
compress' "" c n res = res++[c]++(show n)
compress' (pc:px) c n res
	| c /= pc = compress' px pc 1 (res++[c]++(show n))
	| otherwise = compress' px pc (n+1) res

main :: IO ()
main = do
  p<- getLine
  putStrLn $ compress' p '\0' 0 ""

