--  <description>
--  Package ColorRGB contains the basic types and functions to handle RGB Color Space
--  </description>
--  <group>BASE PACKAGES</group>

with StdTypes; use StdTypes;

package ColorRGB is

   ------------------------
   --  TYPE DEFINITIONS  --
   ------------------------

   type Color_RGB is private;
   --  <summary>Color_RGB type stores an RGB Color</summary>

   -------------------------------------
   --  SUPPORTING TEXT I/O FUNCTIONS  --
   -------------------------------------

   procedure Put (Color : in Color_RGB);
   --  <summary>Put procedure to print a Color_RGB</summary>
   --  <parameter name="Color">A Color_RGB</parameter>
   --  <exception>No exception</exception>

   -----------------------------
   --  CONSTRUCTOR FUNCTIONS  --
   -----------------------------

   function ConstructColor_RGB (R, G, B : in SMALL_FLOAT) return Color_RGB;
   --  <summary>function to construct a Color_RGB from three Floats</summary>
   --  <parameter name="R">A SMALL_FLOAT, denoting the Red part of the color</parameter>
   --  <parameter name="G">A SMALL_FLOAT, denoting the Green part of the color</parameter>
   --  <parameter name="B">A SMALL_FLOAT, denoting the Blue part of the color</parameter>
   --  <exception>No exception</exception>

   ----------------------------
   --  COLOR_RGB OPERATIONS  --
   ----------------------------
   function "+" (Left, Right : in Color_RGB) return Color_RGB;
   --  <summary>function to add two Color_RGB variables.
   --  It returns a Color_RGB</summary>
   --  <parameter name="Left">A Color_RGB</parameter>
   --  <parameter name="Right">A Color_RGB</parameter>
   --  <exception>No exception</exception>

   function "*" (Left, Right : in Color_RGB) return Color_RGB;
   --  <summary>function to multiply two Color_RGB variables.
   --  It returns a Color_RGB</summary>
   --  <parameter name="Left">A Color_RGB</parameter>
   --  <parameter name="Right">A Color_RGB</parameter>
   --  <exception>No exception</exception>

   function Color_RGB_To_X11 (Color : in Color_RGB) return Integer;
   --  <summary>function to convert a Color_RGB to an X11 Integer Color.
   --  It returns an Integer</summary>
   --  <parameter name="Color">A Color_RGB</parameter>
   --  <exception>No exception</exception>

   ----------------------------
   --  SOME COLOR CONSTANTS  --
   ----------------------------

   RGB_White : constant Color_RGB;
   RGB_Black : constant Color_RGB;
   RGB_Red   : constant Color_RGB;
   RGB_Green : constant Color_RGB;
   RGB_Blue  : constant Color_RGB;

private

   type Color_RGB is record
      R, G, B : SMALL_FLOAT := 0.0;
   end record;

   RGB_White : constant Color_RGB := (R => 1.0, G => 1.0, B => 1.0);
   RGB_Black : constant Color_RGB := (R => 0.0, G => 0.0, B => 0.0);
   RGB_Red   : constant Color_RGB := (R => 1.0, G => 0.0, B => 0.0);
   RGB_Green : constant Color_RGB := (R => 0.0, G => 1.0, B => 0.0);
   RGB_Blue  : constant Color_RGB := (R => 0.0, G => 0.0, B => 1.0);

end ColorRGB;
