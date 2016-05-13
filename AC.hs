data DAState = Lambda | State String deriving(Show)
data Config = Config (DAState, [(Char,DAState)]) deriving(Show)
data BackEdge = BackEdge (DAState,DAState) deriving(Show)

instance Eq DAState where
    Lambda  == Lambda   = True
    State a == State b  = a == b
    _       == _        = False

instance Eq Config where
    Config (a,b) == Config (c,d) = (a==c) && (b==d)

mySubstring :: String -> Int -> Int -> String
mySubstring sez begin end = drop begin (take end sez)

erase :: DAState -> [Config] -> [Config]
erase state cfgs = filter (\ (Config a) -> (fst a)/=state) cfgs

lookfor :: DAState -> [Config] -> Maybe Config
lookfor state [] = Nothing
lookfor state (c:confs) = if state == cstate then Just c
                        else lookfor state confs
                    where Config (cstate,nic) = c

gen :: String -> [Config] -> Int -> [Config]
gen word c i = f2 word c i
    where f2 w c 0 = if result == Nothing then (Config (Lambda, [(head (mySubstring w 0 1),(State (mySubstring w 0 1)))] )):c
                    else (  Config ( first_state , ( (head (mySubstring w 0 1), State (mySubstring w 0 1)) ) : (second_cond) )  ):(erase Lambda c) -- odeber result, pridej do resultiho seznamu a vrat to zpet
                        where   result = lookfor Lambda c
                                convert (Just a) = a
                                Config (first_state,second_cond) = convert result
          f2 w c i = if result == Nothing then ( Config( (State (mySubstring w 0 i)), [ (head (mySubstring w i (i+1)) , (State (mySubstring w 0 (i+1))))]) ):c
                        else (  Config ( first_state , ( (head (mySubstring w i (i+1)),(State (mySubstring w 0 (i+1)))))  : (second_cond) )  ):(erase first_state c) -- odeber result, pridej do resultiho seznamu a vrat to zpet
                            where   result = lookfor (State (mySubstring w 0 i)) c
                                    convert (Just a) = a
                                    Config (first_state,second_cond) = convert result

genForWord :: String -> [Config] -> Int -> [Config]
genForWord word conf num | num == ( (length word)) = conf
                         | otherwise = genForWord word (gen word conf num) (num+1)

genWords :: [String] -> [Config] -> [Config]
genWords [] conf = conf
genWords (w:word) conf = genWords word (genForWord w conf 0)
