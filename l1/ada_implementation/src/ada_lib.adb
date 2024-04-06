with Ada.Text_IO; use Ada.Text_IO;

package body Ada_Lib is
   function IFactor(N : UInt16) return UInt64 is
      R : UInt64 := 1;
   begin
      for I in UInt16 range 2 .. N loop
         R := R * UInt64(I);
      end loop;
      return R;
   end IFactor;

   function RFactor(N : UInt16) return UInt64 is
   begin
      if N < 2 then
         return 1;
      else
         return UInt64(N) * RFactor(UInt16(N - 1));
      end if;
   end RFactor;

   function IGCD(A, B : UInt64) return UInt64 is
      LocalA, LocalB, T : UInt64 := A;
   begin
      LocalB := B;
      while LocalB /= 0 loop
         T := LocalB;
         LocalB := LocalA mod LocalB;
         LocalA := T;
      end loop;
      return LocalA;
   end IGCD;

   function RGCD(A, B : UInt64) return UInt64 is
   begin
      if B = 0 then
         return A;
      else
         return RGCD(B, A mod B);
      end if;
   end RGCD;

   function ERGCD(A, B : Int64; X, Y : access Int64) return Int64 is
   begin
      if B = 0 then
         X.all := 1;
         Y.all := 0;
         return A;
      else
         declare
            G : Int64;
            X1, Y1 : Int64;
         begin
            G := ERGCD(B, A mod B, X, Y);
            X1 := X.all;
            Y1 := Y.all;
            X.all := Y1;
            Y.all := X1 - Int64(A / B) * Y1; -- Explicit conversion to Int64
            return G;
         end;
      end if;
   end ERGCD;

   function EIGCD(A, B : Int64; X, Y : access Int64) return Int64 is
      X0, Y0, X1, Y1, Temp, Quotient, Remainder, TA, TB : Int64 := 1;
   begin
      Y0 := 0;
      X1 := 0;
      Y1 := 1;
      Temp := 0;
      TA := A;
      TB := B;

      X.all := X0;
      Y.all := Y0;

      while TB /= 0 loop
         Quotient := TA / TB;
         Remainder := TA mod TB;
         TA := TB;
         TB := Remainder;
         Temp := X1;
         X1 := X0 - Quotient * X1;
         X0 := Temp;
         Temp := Y1;
         Y1 := Y0 - Quotient * Y1;
         Y0 := Temp;

         X.all := X0; -- Update the value pointed by X
         Y.all := Y0; -- Update the value pointed by Y
      end loop;

      return TA;
   end EIGCD;

   function RLDES(A, B, C : Int64) return Int64_Pair is
      X, Y : Int64;
   begin
      if A = 0 and B = 0 then
         if C = 0 then
            Put_Line("Infinite Solutions Exist");
         else
            Put_Line("No Solution Exists");
         end if;
      end if;

      declare
         X_Ptr, Y_Ptr : access Int64;
      begin
         X_Ptr := new Int64;
         Y_Ptr := new Int64;
         if X_Ptr = null or Y_Ptr = null then
            Put_Line("Memory allocation failed");
            raise Storage_Error;
         end if;

         if A = 0 and B = 0 then
            Put_Line("Infinite Solutions Exist");
         elsif C mod ERGCD(A, B, X_Ptr, Y_Ptr) /= 0 then
            Put_Line("No Solution Exists");
         else
            X := X_Ptr.all;
            Y := Y_Ptr.all;
         end if;
         return (X, Y);
      end;
   end RLDES;

   function ILDES(A, B, C : Int64) return Int64_Pair is
      X, Y : Int64;
   begin
      if A = 0 and B = 0 then
         if C = 0 then
            Put_Line("Infinite Solutions Exist");
         else
            Put_Line("No Solution Exists");
         end if;
      end if;

      declare
         X_Ptr, Y_Ptr : access Int64;
      begin
         X_Ptr := new Int64;
         Y_Ptr := new Int64;
         if X_Ptr = null or Y_Ptr = null then
            Put_Line("Memory allocation failed");
            raise Storage_Error;
         end if;

         if A = 0 and B = 0 then
            Put_Line("Infinite Solutions Exist");
         elsif C mod EIGCD(A, B, X_Ptr, Y_Ptr) /= 0 then
            Put_Line("No Solution Exists");
         else
            X := X_Ptr.all;
            Y := Y_Ptr.all;
         end if;
         return (X, Y);
      end;
   end ILDES;
end Ada_Lib;