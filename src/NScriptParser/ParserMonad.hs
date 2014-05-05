module NScriptParser.ParserMonad where

{-|
 Define exception monad to do error handling as well as line numbers
-}
data ParseResult a = OkP a | FailedP String
type LineNumber = Int
newtype ParseAction a = ParseAction (String -> LineNumber -> ParseResult a)

runP :: ParseAction a -> String -> LineNumber -> ParseResult a
runP (ParseAction f) = f

instance Monad ParseAction where
  return m = ParseAction $ \_ _ -> OkP m
  m >>= k = ParseAction $ \s l -> case runP m s l of
    OkP a -> runP (k a) s l
    FailedP err -> FailedP err
  fail s = ParseAction $ \_ _ -> FailedP s

getLineNo :: ParseAction LineNumber
getLineNo = ParseAction $ \_ l -> OkP l

returnToken :: (t -> ParseAction a) -> t -> String -> LineNumber -> ParseResult a
returnToken cont tok = runP (cont tok)