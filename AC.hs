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
            if forwardJump == Nothing then stepBack state letter confs (backs,shorts)
            else (convert forwardJump)
    where
        forwardEdge = lookfor state confs
        Config (inState, transitions) = convert forwardEdge
        forwardJump = lookup letter transitions

lookForShort :: DAState -> [ShortEdge] -> Maybe DAState
lookForShort state [] = Nothing
lookForShort state (b:backs) = if state == firstState then Just nextState
                        else lookForShort state backs
                    where ShortEdge (firstState,nextState) = b

getStr :: DAState -> String
getStr st | st == Lambda = ""
          | otherwise = str
          where
            State str = st

forEachBuild :: DAState -> [(Char,DAState)] -> [Config] -> [String] -> ([BackEdge],[ShortEdge]) -> ([BackEdge],[ShortEdge])
forEachBuild iState [] confs finalWords backShort = backShort
forEachBuild iState ((isLetter,sState):rest) confs finalWords (backs,shorts) = forEachBuild iState rest confs finalWords ( (newBack:backs)  , (newShort:shorts))
    where
        zpetI = lookforBack iState backs
        f Nothing = Lambda
        f (Just a) = a
        zState = (acKrok (f zpetI) isLetter confs (backs,shorts))
        newBack = BackEdge (  sState , zState )
        zkratkaZ = f $ lookForShort zState shorts
        strZState = (getStr zState)
        newShort = if strZState == "" then ShortEdge ( sState ,  zkratkaZ )
                    else
                        if elem strZState finalWords then ShortEdge ( sState ,  zState   )
                        else ShortEdge ( sState ,  zkratkaZ   )


workQ :: [Config] -> [Config] -> [String] -> ([BackEdge],[ShortEdge]) -> ([BackEdge],[ShortEdge])
workQ [] confs finalWords (back,short) = (back,short)
workQ (front:queue) confs finalWords (back,short) = workQ (queue ++ addToQ forwEdges confs) confs finalWords (forEachBuild state_i forwEdges confs finalWords (back,short))
    where
        Config (state_i,forwEdges) = front
        addToQ [] confs = []
        addToQ ((letter,state):edges) confs = if result == Nothing then (addToQ edges confs)
                                      else ((convert result):(addToQ edges confs))
            where
                result = lookfor state confs

-- gets a list of words, returns the list of final words, forwardConfigs, backedges and shortedges
treeBuild :: [String] -> ( [String] ,[Config], ([BackEdge],[ShortEdge]) )
treeBuild needles = (needles, confs , backShort )
    where
        confs = (genWords needles [])
        lambdaState = lookfor Lambda confs
        Config (smt, children) = convert lambdaState
        initConf = map (\ (letter, newState) -> convert ( lookfor newState confs ) ) $ children
        backShort = workQ initConf confs needles ([],[])

goThroughShorts :: DAState -> [ShortEdge] -> [String] -> [String] -> [String]
goThroughShorts Lambda _ _ f = f
goThroughShorts state shorts finalWords found = goThroughShorts nextState shorts finalWords newfound
    where
        nextState = f $ lookForShort state shorts
        f Nothing = Lambda
        f (Just a) = a
        newfound = if (elem (getStr state) finalWords) then (getStr state):found
                        else found

acHledej :: DAState -> [String] -> String -> ( [String] ,[Config], ([BackEdge],[ShortEdge]) ) -> [String] -> ( DAState, [String] )
acHledej state _ [] (finalWords, _, (backs,shorts)) found = (state, found ++ (goThroughShorts state shorts finalWords []))
acHledej state (needles) (h:hay) (finalWords, confs, (backs,shorts)) found = acHledej (acKrok state h confs (backs,shorts)) needles hay (finalWords, confs, (backs,shorts)) ( found ++ (goThroughShorts state shorts finalWords []))

ac :: [String] -> String -> [String]
ac needles hay = snd $ acHledej Lambda needles hay (treeBuild needles) []
