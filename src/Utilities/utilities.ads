with Core_Types; use Core_Types;

--<summary>The Utilities package contains some Utility routines</summary>
--<description>The Utilities package contains some Utility routines</description>
--<group>Utilities</group>
package Utilities is

   M_Dart_Version : constant String := "01.02.01.00";
   --<summary>M_Dart_Version contains the version number of M-Dart.
   --it should be replaced by some VCS parameter in the future</summary>

   procedure Opening_Message;
   --<summary>Opening_Message is a procedure to print a start-up message</summary>

   procedure Closing_Message;
   --<summary>Closing_Message is a procedure to print a closing message</summary>

   procedure Debug_Message (Msg : in String; Level : in Integer := 0);
   --<summary>Prints a debug message</summary>

   --  Some statistic storage variables
   Number_Of_Primary_Rays                : Huge_Integer  := 0;
   Number_Of_Reflected_Rays              : Huge_Integer  := 0;
   Number_Of_Transmitted_Rays            : Huge_Integer  := 0;
   Number_Of_Unit_Sphere_Intersections   : Huge_Integer  := 0;
   Number_Of_Unit_Cylinder_Intersections : Huge_Integer  := 0;
   Number_Of_Unit_Cone_Intersections     : Huge_Integer  := 0;
   Number_Of_Unit_Cube_Intersections     : Huge_Integer  := 0;
   Number_Of_CSG_Object_Intersections    : Huge_Integer  := 0;
   Number_Of_Triangle_Intersections      : Huge_Integer  := 0;
   Number_Of_Shadow_Rays                 : Huge_Integer  := 0;
   Number_Of_Unit_Sphere_Hits            : Huge_Integer  := 0;
   Number_Of_Unit_Cylinder_Hits          : Huge_Integer  := 0;
   Number_Of_Unit_Cone_Hits              : Huge_Integer  := 0;
   Number_Of_Unit_Cube_Hits              : Huge_Integer  := 0;
   Number_Of_CSG_Object_Evaluations      : Huge_Integer  := 0;
   Number_Of_Triangle_Hits               : Huge_Integer  := 0;
   Max_Number_Of_Shadepoints             : Large_Integer := 0;
   DEBUG_LEVEL                           : Integer       := 1;
end Utilities;
