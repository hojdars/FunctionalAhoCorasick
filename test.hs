import AC

main =  do
    if  ( acKrok (State "bar") 'a' (genWords ["ara","bar"] []) ([BackEdge (State "ara", State "a"), BackEdge (State "bar" , State "ar")],[]) == (State "ara") )  then print "Test 1 OK" else print "Test 1 FAIL"
    if  ( acKrok (State "ara") 'r' (genWords ["ara","bar"] []) ([BackEdge (State "ara", State "a"), BackEdge (State "bar" , State "ar")],[]) == (State "ar") )  then print "Test 2 OK" else print "Test 2 FAIL" -- this should fail for backing edges
    if  ( stepBack (State "ara") 'a' (genWords ["ara"] []) ([BackEdge (State "ara", State "a")],[])  == (State "a") )  then print "Test 3 OK" else print "Test 3 FAIL"
    if  ( stepBack (State "ara") 'r' (genWords ["ara"] []) ([BackEdge (State "ara", State "a")],[])== (State "ar") )  then print "Test 4 OK" else print "Test 4 FAIL"
    if  ( stepBack (State "ara") 'a' (genWords ["ara"] []) ([BackEdge (State "ara", State "a")],[])== (State "a") )  then print "Test 5 OK" else print "Test 5 FAIL"
    if  ( genWords ["ara","arab","bar","bara","barb"] [] ==  ([Config (State "bar",[('b',State "barb"),('a',State "bara")]),Config (State "ba",[('r',State "bar")]),Config (State "b",[('a',State "ba")]),Config (Lambda,[('b',State "b"),('a',State "a")]),Config (State "ara",[('b',State "arab")]),Config (State "ar",[('a',State "ara")]),Config (State "a",[('r',State "ar")])]) ) then print "Test 6 OK" else print "Test 6 FAIL"
