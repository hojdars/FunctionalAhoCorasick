module KMP where

data DAState = Lambda | State String deriving(Show)

instance Eq DAState where
    Lambda  == Lambda   = True
    State a == State b  = a == b
    _       == _        = False

mySubstring :: String -> Int -> Int -> String
mySubstring sez begin end = drop begin (take end sez)

-- generate states from a word
generateStates :: String -> [DAState]
generateStates word = map (f word) [0..(length word)]
                where
                    f _ 0 = Lambda
                    f w i = State (mySubstring w 0 i)
                    
-- word -> states -> konfigurations (state, read letter, new state)
generateForward :: String -> [DAState] -> [( DAState , Char, DAState )]
generateForward [] _ = []
generateForward (w:word) (st:nextst:states) = (st, w, nextst):(generateForward word (nextst:states))

findStepBack :: DAState -> Char -> [( DAState , Char, DAState )] -> [(DAState,DAState)] -> DAState
findStepBack Lambda letter transitions _ = f (findForward letter Lambda transitions)
                                    where
                                        f Nothing = Lambda
                                        f (Just backState) = backState
findStepBack state letter transitions backs = if (forwardEdge) == Nothing then findStepBack nextBack letter transitions backs
                                            else (f forwardEdge)
                                    where
                                        forwardEdge = findForward letter state transitions
                                        f Nothing = Lambda
                                        f (Just backState) = backState
                                        nextBack = f $ lookup state backs

-- delta function, State + word + konfigurations + backEdges --> new state
deltaOne :: DAState -> Char -> [( DAState , Char, DAState )] -> [(DAState,DAState)] -> DAState
deltaOne state letter transitions backs =   if (forwardEdge) == Nothing then findStepBack state letter transitions backs
                                            else (f forwardEdge)
                                    where
                                        forwardEdge = findForward letter state transitions
                                        f Nothing = Lambda
                                        f (Just backState) = backState

-- finds a forward edge or declares Nothing
findForward :: Char -> DAState -> [( DAState , Char, DAState )] -> Maybe DAState
findForward _ _ [] = Nothing
findForward letter state (t:transitions) =  if findState == state && letter == findLetter then (Just target)
                                            else findForward letter state transitions
                                        where
                                            (findState, findLetter, target) = t

-- reads all the whole word to the end
deltaStar :: DAState -> String -> [( DAState , Char, DAState )] -> [(DAState,DAState)] -> DAState
deltaStar st [] _ backs = st
deltaStar st (w:word) transitions backs = deltaStar ( deltaOne st w transitions backs ) word transitions backs

-- generates a second list with the back edges so the KMP automata can return
generateBackEdges' :: DAState -> String -> [( DAState , Char, DAState )] -> [DAState]
generateBackEdges' st [] _ = [st]
generateBackEdges' st (w:word) transitions = (st : nextSteps)
                                        where
                                            nextState = deltaOne st w transitions []
                                            nextSteps = generateBackEdges' nextState word transitions

-- states, backing edges generated
-- states word konfigurations --> backing edges
generateBackEdges :: [DAState] -> String -> [( DAState , Char, DAState )] -> [(DAState,DAState)]
generateBackEdges states word konfigurations = ( (Lambda,Lambda):(zip (tail states) $ generateBackEdges' Lambda (tail word) konfigurations))

evalWord :: String -> String -> DAState
evalWord word text = deltaStar Lambda text configs backs
                where
                    states = generateStates word
                    configs = generateForward word states
                    backs = generateBackEdges states word configs
