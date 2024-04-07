--  This is a sample Ada test package specification file

with Ada.Text_IO;
with Ada.Integer_Text_IO;

with Ada;

package Test_Lib is
   procedure RunAllTests;

private
   procedure TestFactorial;
   --  Tests the IFactor and RFactor functions

   procedure TestGCD;
   --  Tests the IGCD and RGCD functions

   procedure TestLDE;
   --  Tests the ERGCD, EIGCD, RLDES, and ILDES functions
end Test_Lib;