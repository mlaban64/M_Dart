with Ada.Numerics;

--<summary>The Core_Types package defines the basic scalar data types, constants, supporting functions and derived packages</summary>
--<description>The CoreTypes package defines the basic scalar data types, constants, supporting functions and derived packages</description>
--<group>Core</group>
package Core_Types is

   type Small_Integer is new Short_Integer;
   --<summary>Small_Integer is a 16-bit signed integer</summary>

   procedure Put (N : in Small_Integer);
   --<summary>Prints the value of a Small_Integer</summary>
   --<description>Prints the value of a Small_Integer</description>
   --<parameter name="N">The variable to be printed</parameter>
   --<exception>None at this moment</exception>

   type Normal_Integer is new Integer;
   --<summary>Normal_Integer is a 32-bit signed integer</summary>

   procedure Put (N : in Normal_Integer);
   --<summary>Prints the value of a Normal_Integer</summary>
   --<description>Prints the value of a Normal_Integer</description>
   --<parameter name="N">The variable to be printed</parameter>
   --<exception>None at this moment</exception>

   type Large_Integer is new Long_Integer;
   --<summary>Large_Integer is a 64-bit signed integer</summary>

   procedure Put (N : in Large_Integer);
   --<summary>Prints the value of a Large_Integer</summary>
   --<description>Prints the value of a Large_Integer</description>
   --<parameter name="N">The variable to be printed</parameter>
   --<exception>None at this moment</exception>

   type Huge_Integer is new Long_Long_Integer;
   --<summary>Huge_Integer is a 128-bit signed integer</summary>

   procedure Put (N : in Huge_Integer);
   --<summary>Prints the value of a Huge_Integer</summary>
   --<description>Prints the value of a Huge_Integer</description>
   --<parameter name="N">The variable to be printed</parameter>
   --<exception>None at this moment</exception>

   type Small_Float is new Float;
   --<summary>Small_Float is a 32-bit signed float</summary>

   procedure Put (N : in Small_Float);
   --<summary>Prints the value of a Small_Float</summary>
   --<description>Prints the value of a Small_Float</description>
   --<parameter name="N">The variable to be printed</parameter>
   --<exception>None at this moment</exception>

   type Normal_Float is new Long_Float;
   --<summary>Normal_Float is a 64-bit signed float</summary>

   procedure Put (N : in Normal_Float);
   --<summary>Prints the value of a Normal_Float</summary>
   --<description>Prints the value of a Normal_Float</description>
   --<parameter name="N">The variable to be printed</parameter>
   --<exception>None at this moment</exception>

   type Large_Float is new Long_Long_Float;
   --<summary>Large_Float is a 128-bit signed float</summary>

   procedure Put (N : in Large_Float);
   --<summary>Prints the value of a Large_Float</summary>
   --<description>Prints the value of a Large_Float</description>
   --<parameter name="N">The variable to be printed</parameter>
   --<exception>None at this moment</exception>

   --
   --  Some useful constants
   --
   Ray_PI      : constant Large_Float := Ada.Numerics.Pi;
   Ray_2PI     : constant Large_Float := 2.0 * Ray_PI;
   Ray_PI_Inv  : constant Large_Float := 1.0 / Ray_PI;
   Ray_2PI_Inv : constant Large_Float := 1.0 / Ray_2PI;
   Ray_PI_div2 : constant Large_Float := Ray_PI / 2.0;
   Ray_PI_div3 : constant Large_Float := Ray_PI / 3.0;
   Ray_PI_div4 : constant Large_Float := Ray_PI / 4.0;
   Ray_Epsilon : constant Large_Float := 0.000_000_001;

end Core_Types;