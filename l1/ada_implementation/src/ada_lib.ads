package Ada_Lib is
   type UInt64 is mod 2**64;
   type UInt16 is mod 2**16;
   type Int64 is range -2**63 .. 2**63 - 1;

   type Int64_Pair is record
      X, Y : Int64;
   end record
      with Convention => C;
   
   function IFactor(N : UInt16) return UInt64
      with
         Export => True,
         Convention => C,
         External_Name => "IFactor";
   
   function RFactor(N : UInt16) return UInt64
      with
         Export => True,
         Convention => C,
         External_Name => "RFactor";
   function IGCD(A, B : UInt64) return UInt64
      with
         Export => True,
         Convention => C,
         External_Name => "IGCD";
   function RGCD(A, B : UInt64) return UInt64
      with
         Export => True,
         Convention => C,
         External_Name => "RGCD";
   function ERGCD(A, B : Int64; X, Y : access Int64) return Int64;
   function EIGCD(A, B : Int64; X, Y : access Int64) return Int64;
   function RLDES(A, B, C : Int64) return Int64_Pair
      with
         Export => True,
         Convention => C,
         External_Name => "RLDES";
   function ILDES(A, B, C : Int64) return Int64_Pair
      with
         Export => True,
         Convention => C,
         External_Name => "ILDES";
end Ada_Lib;