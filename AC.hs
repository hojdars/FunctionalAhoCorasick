module AC where
import Data.List

data DAState = Lambda | State String deriving(Show)
data Config = Config (DAState, [(Char,DAState)]) deriving(Show)
data BackEdge = BackEdge (DAState,DAState) deriving(Show)
data ShortEdge = ShortEdge (DAState,DAState) deriving(Show)

instance Eq DAState where
    Lambda  == Lambda   = True
    State a == State b  = a == b
    _       == _        = False

instance Eq Config where
    Config (a,b) == Config (c,d) = (a==c) && (b==d)

convert :: Maybe a -> a
convert (Just a) = a

mySubstring :: String -> Int -> Int -> String
mySubstring sez begin end = drop begin (take end sez)

-- erase all Config (state, _) with the corresponding state
-- note: that really should be only one always, as we do not create multiple configs for the same (state,_)
erase :: DAState -> [Config] -> [Config]
erase state cfgs = filter (\ (Config a) -> (fst a)/=state) cfgs

-- prelude's lookup for Config type
lookfor :: DAState -> [Config] -> Maybe Config
lookfor state [] = Nothing
lookfor state (c:confs) = if state == cstate then Just c
                        else lookfor state confs
                    where Config (cstate,nic) = c

-- generates one forward edge
-- it looks up the config in config list for the state we want to create, if it already exists (like in barb + barc test, bar already exists)
-- we add the edge into the new state to the list, if it doesn't we create the whole config
gen :: String -> [Config] -> Int -> [Config]
gen w c 0 = if result == Nothing then (Config (Lambda, [(head (mySubstring w 0 1),(State (mySubstring w 0 1)))] )):c
            else (  Config ( first_state , nub (( (head (mySubstring w 0 1), State (mySubstring w 0 1)) ) : (second_cond)) )  ):(erase Lambda c) -- odeber result, pridej do resultiho seznamu a vrat to zpet
                where   result = lookfor Lambda c
                        Config (first_state,second_cond) = convert result
gen w c i = if result == Nothing then ( Config( (State (mySubstring w 0 i)), [ (head (mySubstring w i (i+1)) , (State (mySubstring w 0 (i+1))))]) ):c
            else (  Config ( first_state ,  nub ( ( (head (mySubstring w i (i+1)), (State (mySubstring w 0 (i+1)))))  : (second_cond) ) )  ):(erase first_state c) -- odeber result, pridej do resultiho seznamu a vrat to zpet
                where   result = lookfor (State (mySubstring w 0 i)) c
                        Config (first_state,second_cond) = convert result

-- generates states and forward edges for one word
genForWord :: String -> [Config] -> Int -> [Config]
genForWord word conf num | num == ( (length word)) = conf
                         | otherwise = genForWord word (gen word conf num) (num+1)

-- generates forward edges and all the states
genWords :: [String] -> [Config] -> [Config]
genWords [] conf = conf
genWords (w:word) conf = genWords word (genForWord w conf 0)


-- takes one forward step from given state over given letter, need to provide Configurations
forwardStep :: DAState -> Char -> [Config] -> DAState
forwardStep state letter confs = if result == Nothing then Lambda
                            else
                                 if edge == Nothing then Lambda
                                 else convert edge
    where
        result = lookfor state confs
        Config (first, transitions) = convert result
        edge = lookup letter transitions

-- prelude's lookup for BackEdge type
lookforBack :: DAState -> [BackEdge] -> Maybe DAState
lookforBack state [] = Nothing
lookforBack state (b:backs) = if state == firstState then Just nextState
                        else lookforBack state backs
                    where BackEdge (firstState,nextState) = b

stepBack :: DAState -> Char -> [Config] -> ([BackEdge],[ShortEdge]) -> DAState
stepBack Lambda letter confs (backs,shorts) = forwardStep Lambda letter confs
stepBack state letter confs (b,s) = if forwardEdge == Nothing then stepBack nextBack letter confs (b,s)
                                    else
                                        if (lookup letter transitions) == Nothing then stepBack nextBack letter confs (b,s)
                                        else forwardStep state letter confs
    where
        forwardEdge = lookfor state confs
        Config (inState, transitions) = convert forwardEdge
        f Nothing = Lambda
        f (Just a) = a
        nextBack = f $ lookforBack state b

-- ACKrok from MJ's book
acKrok :: DAState -> Char -> [Config] -> ([BackEdge],[ShortEdge]) -> DAState
acKrok state letter confs (backs,shorts) =
        if forwardEdge == Nothing then stepBack state letter confs (backs,shorts)
        else
            if forwardJump == Nothing then Lambda
            else (convert forwardJump)
    where
    forwardEdge = lookfor state confs
    Config (inState, transitions) = convert forwardEdge
    forwardJump = lookup letter transitions

-- TODO : do fronty budu davat dvojice (pismeno, state) - pismeno kvuli "pismenu na hrane is"
-- generates backs and shorts
workQ :: [(Char, DAState)] -> [Config] -> ([BackEdge],[ShortEdge]) -> ([BackEdge],[ShortEdge])
workQ (f:queue) confs (back,short) = ([],[])
