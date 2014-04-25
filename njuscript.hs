import System.Environment

main :: IO()
main = getArgs >>= print . sayHello . head

sayHello s = "Hello, " ++ s ++ "!"
