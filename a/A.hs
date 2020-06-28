module A where
import B

main :: IO ()
main = do
  putStrLn "Hello, World!"
  print foo
  print a
a = 1
