import KMP

main =  do
    if  (deltaStar (Lambda) "aho" (generateForward "ahoj" (generateStates "ahoj")) == (State "aho") )  then print "Test 1 OK" else print "Test 1 FAIL"
    if  (deltaStar (Lambda) "ahoaa" (generateForward "ahoj" (generateStates "ahoj")) == (State "a") )  then print "Test 2 OK" else print "Test 2 FAIL" -- this should fail for backing edges
    if  (deltaStar (Lambda) "xxx" (generateForward "ahoj" (generateStates "ahoj")) == (Lambda) )  then print "Test 3 OK" else print "Test 3 FAIL"
