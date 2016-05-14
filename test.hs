import AC

main =  do
    if  ( ac ["baraba","arab","ar"] "barabarab" == ["ar","arab","baraba","ar","arab"] )  then print "Test 1 OK" else print "Test 1 FAIL"
    if  ( ac ["baraba","arab","ar"] "barabaraba" == ["ar","arab","baraba","ar","arab","baraba"] )  then print "Test 2 OK" else print "Test 2 FAIL" -- this should fail for backing edges
    if  ( ac ["baraba","arab","ar","barabas"] "barabarabas" == ["ar","arab","baraba","ar","arab","baraba","barabas"] )  then print "Test 3 OK" else print "Test 3 FAIL"
    if  ( ac ["baraba","arab","ar","barabas","ab"] "barabarabas"== ["ar","ab","arab","baraba","ar","ab","arab","baraba","barabas"] )  then print "Test 4 OK" else print "Test 4 FAIL"
