cd src
gcc -c -fdump-ada-spec -C lib.h
cd ..
gprbuild -p -P Test_C_Wrp_Prj.gpr
./test 