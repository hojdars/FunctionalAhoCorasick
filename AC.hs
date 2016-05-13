data DAState = Lambda | State String deriving(Show)

instance Eq DAState where
    Lambda  == Lambda   = True
    State a == State b  = a == b
    _       == _        = False

mySubstring :: String -> Int -> Int -> String
mySubstring sez begin end = drop begin (take end sez)

generateStates :: String -> [DAState]
generateStates word = map (f word) [0..(length word)]
                where
                    f _ 0 = Lambda
                    f w i = State (mySubstring w 0 i)
