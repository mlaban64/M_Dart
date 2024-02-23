--  <description>
--  Package StdTypes contains a collection of types to be used throughout
--  the projects. It is intended to hide and implementation-dependant types
--  to make your code more portable
--  </description>
--  <group>BASE PACKAGES</group>

with Ada.Numerics;

package StdTypes is

   type Integer_16 is new Short_Integer;
   --  <summary>Integer_16 type stores a 16-bit signed Integer</summary>

   type Integer_32 is new Integer;
   --  <summary>Integer_32 type stores a 32-bit signed Integer</summary>

   type Integer_64 is new Long_Long_Integer;
   --  <summary>Integer_64 type stores a 64-bit signed Integer</summary>

   type Float_32 is new Float;
   --  <summary>Float_32 type stores a 32-bit Float</summary>

   type Float_64 is new Long_Float;
   --  <summary>Float_64 type stores a 64-bit Float</summary>

   type Float_96 is new Long_Long_Float;
   --  <summary>Float_96 type stores a 96-bit Float. This may be specific to X86 architectures and
   --  may not be a 96-bit on another architecture</summary>

   subtype SMALL_INTEGER is Integer_16;
   --  <summary>SMALL_INTEGER type</summary>

   subtype LARGE_INTEGER is Integer_32;
   --  <summary>LARGE_INTEGER type</summary>

   subtype HUGE_INTEGER is Integer_64;
   --  <summary>HUGE_INTEGER type</summary>

   subtype SMALL_FLOAT is Float_32;
   --  <summary>SMALL_FLOAT type</summary>

   subtype LARGE_FLOAT is Float_64;
   --  <summary>LARGE_FLOAT type</summary>

   subtype HUGE_FLOAT is Float_96;
   --  <summary>HUGE_FLOAT type</summary>

   procedure Put (X : in Integer_16);
   --  <summary>Put procedure to print an Integer_16 or its subtypes</summary>
   --  <parameter name="X">An Integer_16</parameter>
   --  <exception>No exception</exception>

   procedure Put (X : in Integer_32);
   --  <summary>Put procedure to print an Integer_32 or its subtypes</summary>
   --  <parameter name="X">An Integer_32</parameter>
   --  <exception>No exception</exception>

   procedure Put (X : in Integer_64);
   --  <summary>Put procedure to print an Integer_64 or its subtypes</summary>
   --  <parameter name="X">An Integer_64</parameter>
   --  <exception>No exception</exception>

   procedure Put (X : in Float_32);
   --  <summary>Put procedure to print a Float_32 or its subtypes</summary>
   --  <parameter name="X">A Float_32</parameter>
   --  <exception>No exception</exception>

   procedure Put (X : in Float_64);
   --  <summary>Put procedure to print a Float_64 or its subtypes</summary>
   --  <parameter name="X">A Float_32</parameter>
   --  <exception>No exception</exception>

   procedure Put (X : in Float_96);
   --  <summary>Put procedure to print a Float_96 or its subtypes</summary>
   --  <parameter name="X">A Float_32</parameter>
   --  <exception>No exception</exception>

   ----------------------
   --  SOME CONSTANTS  --
   ----------------------
   MyPI: constant LARGE_FLOAT := Ada.Numerics.Pi;
   MyPI2: constant LARGE_FLOAT := 2.0 * MyPI;
   MyPI_Inv: constant LARGE_FLOAT := 1.0 / MyPI;
   MyPI2_Inv: constant LARGE_FLOAT := 1.0 / MyPI2;
   MyPI_Half: constant LARGE_FLOAT := MyPI / 2.0;
   MyPI_Third: constant LARGE_FLOAT := MyPI / 3.0;
   MyPI_Quart: constant LARGE_FLOAT := MyPI / 4.0;

end StdTypes;
