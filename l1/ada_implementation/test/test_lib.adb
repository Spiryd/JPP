with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada_Lib; use Ada_Lib;

package body Test_Lib is
   procedure RunAllTests is
   begin
      Put_Line("Running tests...");

      TestFactorial;
      TestGCD;
      TestLDE;

      Put_Line("All tests completed.");
   end RunAllTests;

   procedure TestFactorial is
      N : UInt16;
      Result : UInt64;
   begin
      Put_Line("Testing IFactor...");
      N := 5;
      Result := IFactor(N);
      Put_Line("IFactor(" & UInt16'Image(N) & ") = " & UInt64'Image(Result));
      pragma Assert (Result = 120); -- 5!

      Put_Line("Testing RFactor...");
      N := 5;
      Result := RFactor(N);
      Put_Line("RFactor(" & UInt16'Image(N) & ") = " & UInt64'Image(Result));
      pragma Assert (Result = 120); -- 5!
   exception
      when Assertion_Error =>
         Put_Line("Assertion failed in TestFactorial.");
         Put_Line("Expected: 120, Actual: " & UInt64'Image(Result));
         raise; -- Re-raise the exception to terminate the program
   end TestFactorial;

   procedure TestGCD is
      A, B : UInt64;
      Result : UInt64;
   begin
      Put_Line("Testing IGCD...");
      A := 24;
      B := 36;
      Result := IGCD(A, B);
      Put_Line("IGCD(" & UInt64'Image(A) & ", " & UInt64'Image(B) & ") = " & UInt64'Image(Result));
      pragma Assert (Result = 12); -- GCD(24, 36) = 12

      Put_Line("Testing RGCD...");
      Result := RGCD(A, B);
      Put_Line("RGCD(" & UInt64'Image(A) & ", " & UInt64'Image(B) & ") = " & UInt64'Image(Result));
      pragma Assert (Result = 12); -- GCD(24, 36) = 12
   exception
      when Assertion_Error =>
         Put_Line("Assertion failed in TestGCD.");
         Put_Line("Expected: 12, Actual: " & UInt64'Image(Result));
         raise; -- Re-raise the exception to terminate the program
   end TestGCD;

   procedure TestLDE is
      A, B, C : Int64;
      Result : Int64_Pair;
   begin
      Put_Line("Testing RLDES and ILDES...");
      A := 24;
      B := 36;
      C := 12;
      Result := RLDES(A, B, C);
      Put_Line("1. RLDES(" & Int64'Image(A) & ", " & Int64'Image(B) & ", " & Int64'Image(C) & ") = (" & Int64'Image(Result.X) & ", " & Int64'Image(Result.Y) & ")");
      pragma Assert (Result.X = -1 and Result.Y = 1); -- ERGCD(24, 36) = 12, RLDES(24, 36, 12) = (1, -1)

      Result := ILDES(A, B, C);
      Put_Line("1. ILDES(" & Int64'Image(A) & ", " & Int64'Image(B) & ", " & Int64'Image(C) & ") = (" & Int64'Image(Result.X) & ", " & Int64'Image(Result.Y) & ")");
      pragma Assert (Result.X = -1 and Result.Y = 1); -- EIGCD(24, 36) = 12, ILDES(24, 36, 12) = (1, -1)

      A := 91;
      B := 35;
      C := 7;
      Result := RLDES(A, B, C);
      Put_Line("2. RLDES(" & Int64'Image(A) & ", " & Int64'Image(B) & ", " & Int64'Image(C) & ") = (" & Int64'Image(Result.X) & ", " & Int64'Image(Result.Y) & ")");
      pragma Assert (Result.X = 2 and Result.Y = -5); -- ERGCD(24, 36) = 12, RLDES(24, 36, 12) = (1, -1)

      Result := ILDES(A, B, C);
      Put_Line("2. ILDES(" & Int64'Image(A) & ", " & Int64'Image(B) & ", " & Int64'Image(C) & ") = (" & Int64'Image(Result.X) & ", " & Int64'Image(Result.Y) & ")");
      pragma Assert (Result.X = 2 and Result.Y = -5); -- EIGCD(24, 36) = 12, ILDES(24, 36, 12) = (1, -1)
   exception
      when Assertion_Error =>
         Put_Line("Assertion failed in TestLDE.");
         Put_Line("Expected: (1, -1), Actual: (" & Int64'Image(Result.X) & ", " & Int64'Image(Result.Y) & ")");
         raise; -- Re-raise the exception to terminate the program
   end TestLDE;

end Test_Lib;