gcc tester.c rec.c -o tester
echo "Recursion"
./tester
gcc tester.c iter.c -o tester
echo "Iteration"
./tester
