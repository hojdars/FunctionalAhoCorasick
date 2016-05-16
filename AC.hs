module AC where
import Data.List

data DAState = Lambda | State String deriving(Show)
data Config = Config (DAState, [(Char,DAState)]) deriving(Show)
type BackEdge = (DAState,DAState) 
type ShortEdge = (DAState,DAState)

instance Eq Config where Config (a,b) == Config (c,d) = (a==c) && (b==d)
instance Eq DAState where
    Lambda  == Lambda   = True
    State a == State b  = a == b
    _       == _        = False

convert :: Maybe a -> a
convert (Just a) = a

convertLambda :: Maybe DAState -> DAState
convertLambda Nothing = Lambda
convertLambda (Just a) = a

getStr :: DAState -> String
getStr st | st == Lambda = ""
          | otherwise = str
          where State str = st

mySubstring :: String -> Int -> Int -> String
mySubstring sez begin end = drop begin (take end sez)

-- erase all Config (state, _) with the corresponding state
-- note: that really should be only one always, as we do not create multiple configs for the same (state,_)
erase :: DAState -> [Config] -> [Config]
erase state cfgs = filter (\ (Config a) -> (fst a)/=state) cfgs

lookfor :: DAState -> [Config] -> Maybe Config -- prelude's lookup for Config type
lookfor state [] = Nothing
lookfor state (c:confs) | state == cstate = Just c 
                        | otherwise       = lookfor state confs
                    where Config (cstate,nic) = c

-- generates one forward edge
-- it looks up the config in config list for the state we want to create, if it already exists (like in barb + barc test, bar already exists)
-- we add the edge into the new state to the list, if it doesn't we create the whole config
gen :: String -> [Config] -> Int -> [Config]
gen w c 0 | result == Nothing = (Config (Lambda, [(head (mySubstring w 0 1),(State (mySubstring w 0 1)))] )):c
          | otherwise         = (  Config ( first_state , nub (( (head (mySubstring w 0 1), State (mySubstring w 0 1)) ) : (second_cond)) )  ):(erase Lambda c) -- odeber result, pridej do resultiho seznamu a vrat to zpet
                where   result = lookfor Lambda c
                        Config (first_state,second_cond) = convert result
gen w c i | result == Nothing = ( Config( (State (mySubstring w 0 i)), [ (head (mySubstring w i (i+1)) , (State (mySubstring w 0 (i+1))))]) ):c
          | otherwise         = (  Config ( first_state ,  nub ( ( (head (mySubstring w i (i+1)), (State (mySubstring w 0 (i+1)))))  : (second_cond) ) )  ):(erase first_state c) -- odeber result, pridej do resultiho seznamu a vrat to zpet
                where   result = lookfor (State (mySubstring w 0 i)) c
                        Config (first_state,second_cond) = convert result

-- generates forward edges and all the states
genWords :: [String] -> [Config] -> [Config]
genWords [] conf = conf
genWords (w:word) conf = genWords word (genForWord w conf 0)
    where genForWord word conf num  | num == ( (length word)) = conf -- generates states and forward edges for one word
                                    | otherwise = genForWord word (gen word conf num) (num+1)

-- takes one forward step from given state over given letter, need to provide Configurations
forwardStep :: DAState -> Char -> [Config] -> DAState
forwardStep state letter confs | result == Nothing = Lambda
                               | edge == Nothing   = Lambda
                               | otherwise         = convert edge
    where
        result = lookfor state confs
        Config (first, transitions) = convert result
        edge = lookup letter transitions

-- a function for the step back, it tries to go forward, if it can it does, else it jumps back once more and repeats
-- either it lands in lambda or it succeeds trying
stepBack :: DAState -> Char -> [Config] -> ([BackEdge],[ShortEdge]) -> DAState
stepBack Lambda letter confs (backs,shorts) = forwardStep Lambda letter confs
stepBack state letter confs (b,s) | forwardEdge == Nothing                 = stepBack nextBack letter confs (b,s)
                                  | (lookup letter transitions) == Nothing = stepBack nextBack letter confs (b,s)
                                  | otherwise                              = forwardStep state letter confs
    where
        forwardEdge = lookfor state confs
        Config (inState, transitions) = convert forwardEdge
        nextBack = convertLambda $ lookup state b

-- one step of the automata, goes directly if it can else it backtracks
acStep :: DAState -> Char -> [Config] -> ([BackEdge],[ShortEdge]) -> DAState
acStep state letter confs (backs,shorts) 
         | forwardEdge == Nothing = stepBack state letter confs (backs,shorts) -- if there exists at least one edge from the current node
         | forwardJump == Nothing = stepBack state letter confs (backs,shorts) -- if there is no edge from this node for this letter, we backtrack
         | otherwise              = (convert forwardJump) -- else we travel to this new node ~ state
    where
        forwardEdge = lookfor state confs
        Config (inState, transitions) = convert forwardEdge
        forwardJump = lookup letter transitions

-- builds the backs and shorts from the given node
forEachBuild :: DAState -> [(Char,DAState)] -> [Config] -> [String] -> ([BackEdge],[ShortEdge]) -> ([BackEdge],[ShortEdge])
forEachBuild iState [] confs finalWords backShort = backShort
forEachBuild iState ((isLetter,sState):rest) confs finalWords (backs,shorts) = forEachBuild iState rest confs finalWords ( (newBack:backs)  , (newShort:shorts))
    where
        zpetI = lookup iState backs
        zState = (acStep ( convertLambda zpetI) isLetter confs (backs,shorts))
        newBack = (  sState , zState )
        zkratkaZ = convertLambda $ lookup zState shorts
        strZState = (getStr zState)
        newShort = if strZState == "" then ( sState ,  zkratkaZ )
                    else if elem strZState finalWords then ( sState ,  zState   )
                         else ( sState ,  zkratkaZ   )

-- constructs the back and short edges
-- if we reached the end we return our accumulator
-- else we add the children of the current node to the queue and call forEachBuild to build the backs&shorts from this node
workQ :: [Config] -> [Config] -> [String] -> ([BackEdge],[ShortEdge]) -> ([BackEdge],[ShortEdge])
workQ [] confs finalWords (back,short) = (back,short)
workQ (front:queue) confs finalWords (back,short) = workQ (queue ++ addToQ forwEdges confs) confs finalWords (forEachBuild state_i forwEdges confs finalWords (back,short))
    where
        Config (state_i,forwEdges) = front
        addToQ [] confs = [] -- we need to add to the queue all the children of the current node
        addToQ ((letter,state):edges) confs = if result == Nothing then (addToQ edges confs)
                                              else ((convert result):(addToQ edges confs))
            where result = lookfor state confs

-- gets a list of words, returns the list of final words, forwardConfigs, backedges and shortedges, using all the functions listed above
treeBuild :: [String] -> ( [String] ,[Config], ([BackEdge],[ShortEdge]) )
treeBuild needles = (needles, confs , backShort )
    where
        confs = (genWords needles []) -- generate the forward edges and trie structure
        lambdaState = lookfor Lambda confs -- we find Lambda -> the root
        Config (smt, children) = convert lambdaState
        initConf = foldr (\ (l,st) acc -> let res = (lookfor st confs) in if res == Nothing then acc else ((convert res):acc) ) [] children -- for each child of root, we add his children to the queue
        backShort = workQ initConf confs needles ([],[]) -- we construct the back and short edges

-- function that goes through all the short jumps whenever we arrive to a node, checking all the possible occurences of some of the needles
goThroughShorts :: DAState -> [ShortEdge] -> [String] -> [String] -> [String]
goThroughShorts Lambda _ _ found = found
goThroughShorts state shorts finalWords found = goThroughShorts nextState shorts finalWords newfound
    where
        nextState = convertLambda $ lookup state shorts
        newfound = if (elem (getStr state) finalWords) then (getStr state):found else found

-- we do acStep for the automata, each time checking all the shorts via goThroughShorts
acSearch :: DAState -> [String] -> String -> ( [String] ,[Config], ([BackEdge],[ShortEdge]) ) -> [String] -> ( DAState, [String] )
acSearch state _ [] (finalWords, _, (backs,shorts)) found = (state, found ++ (goThroughShorts state shorts finalWords []))
acSearch state (needles) (h:hay) (finalWords, confs, (backs,shorts)) found = acSearch (acStep state h confs (backs,shorts)) needles hay (finalWords, confs, (backs,shorts)) ( found ++ (goThroughShorts state shorts finalWords []))

ac :: [String] -> String -> [String]
ac needles hay = snd $ acSearch Lambda needles hay (treeBuild needles) []
