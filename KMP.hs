-- gets a string, start and end, returns the substring
mySubstring :: String -> Int -> Int -> String -> String
mySubstring (x:txt) s t acc =   if s == 0 && t > 0 then mySubstring txt s (t-1) (x:acc)
                                else if s==0 && t==0 then reverse acc
                                else if s > 0 then mySubstring txt (s-1) (t-1) (acc)
                                else error "Error in generating substring"
mySubstring [] _ _ acc = reverse acc

-- generate states from a word
generateStates :: String -> [String]
generateStates str = generateStates' str 0 []

generateStates' :: String -> Int -> [String] -> [String]
generateStates' str len acc =  if (len-1) == (length str) then (reverse acc)
                            else generateStates' str (len+1) ((mySubstring str 0 len [] ):acc)

-- word -> states -> konfigurations (state, read letter, new state)
generateForward :: String -> [String] -> [( String , Char, String )]
generateForward [] _ = []
generateForward (w:word) (st:nextst:states) = (st, w, nextst):(generateForward word (nextst:states))

-- delta function, State + word --> new state
deltaOne :: String -> Char -> [( String , Char, String )] -> String
deltaOne state letter [] = ""
deltaOne state letter (tr:transitions) = if st == state && letter == lt then target
                                        else deltaOne state letter transitions
    where (st,lt,target) = tr
