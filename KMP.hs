module KMP where

data DAState = Lambda | State String deriving(Show)

instance Eq DAState where
    Lambda  == Lambda   = True
    State a == State b  = a == b
    _       == _        = False

-- gets a string, start and end, returns the substring
mySubstring :: String -> Int -> Int -> String -> String
mySubstring (x:txt) s t acc =   if s == 0 && t > 0 then mySubstring txt s (t-1) (x:acc)
                                else if s==0 && t==0 then reverse acc
                                else if s > 0 then mySubstring txt (s-1) (t-1) (acc)
                                else error "Error in generating substring"
mySubstring [] _ _ acc = reverse acc

-- generate states from a word
generateStates :: String -> [DAState]
generateStates str = generateStates' str 0 []

generateStates' :: String -> Int -> [DAState] -> [DAState]
generateStates' str len acc=if (len-1) == (length str) then (reverse acc)
                            else
                                if newS == ""   then generateStates' str (len+1) (Lambda:acc)
                                                else generateStates' str (len+1) (( State newS):acc)
                            where newS = (mySubstring str 0 len [] )

-- word -> states -> konfigurations (state, read letter, new state)
generateForward :: String -> [DAState] -> [( DAState , Char, DAState )]
generateForward [] _ = []
generateForward (w:word) (st:nextst:states) = (st, w, nextst):(generateForward word (nextst:states))

-- delta function, State + word --> new state
deltaOne :: DAState -> Char -> [( DAState , Char, DAState )] -> DAState
deltaOne state letter [] = Lambda -- TODO: zpetne hrany here
deltaOne state letter (tr:transitions) = if st == state && letter == lt then target
                                        else deltaOne state letter transitions
    where (st,lt,target) = tr

deltaStar :: DAState -> String -> [( DAState , Char, DAState )] -> DAState
deltaStar st [] _ = st
deltaStar st (w:word) transitions = deltaStar ( deltaOne st w transitions ) word transitions
