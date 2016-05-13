data DAState = Lambda | State String deriving(Show)

data Config = Config (DAState, Char, DAState) deriving(Show)

instance Eq DAState where
    Lambda  == Lambda   = True
    State a == State b  = a == b
    _       == _        = False

mySubstring :: String -> Int -> Int -> String
mySubstring sez begin end = drop begin (take end sez)

generateForward :: [String] -> [Config]
generateForward [] = []
generateForward (n:needles) = (generateOne n) ++ (generateForward needles)
    where
    generateOne word = map (f2 word) [0..( (length word)-1)]
                    where
                        f2 w 0 = Config (Lambda, head (mySubstring w 0 1),(State (mySubstring w 0 1)))
                        f2 w i = Config ((State (mySubstring w 0 i)), head (mySubstring w i (i+1)),(State (mySubstring w 0 (i+1))))
