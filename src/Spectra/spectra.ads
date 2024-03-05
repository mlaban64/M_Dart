with Core_Types; use Core_Types;

--<summary>The Spectra package contains the Abstract Data Types for simulation of spectra and colors</summary>
--<description>The Spectra package contains the Abstract Data Types for simulation of spectra and colors</description>
--<group>Rendering</group>
package Spectra is

   -------------------
   -- ADT RGB_Spectrum
   -------------------
   type RGB_Spectrum is private;
   --<summary>ADT of a spectrum with floating point R, G and B values</summary>

   type XYZ_Spectrum is private;
   --<summary>ADT of a spectrum with floating point X, Y and Z values, to support the CIE XYZ space</summary>

   type xyY_Spectrum is private;
   --<summary>ADT of a spectrum with floating point x, y and Y values, to support the xyY space</summary>

   function Construct_RGB_Spectrum (R, G, B : in Small_Float) return RGB_Spectrum;
   --<summary>Initializes an RGB_Spectrum with the RGB values passed in</summary>
   --<description>Initializes an RGB_Spectrum with the RGB values passed in</description>
   --<parameter name="R">The Red value of the spectrum</parameter>
   --<parameter name="G">The Green value of the spectrum</parameter>
   --<parameter name="B">The Blue value of the spectrum</parameter>
   --<exception>None at this moment</exception>

   procedure Put (Spectrum : in RGB_Spectrum; Msg : in String := "RGB_Spectrum = ");
   --<summary>Prints an RGB_Spectrum</summary>
   --<description>Prints an RGB_Spectrum to the screen in a readable format, prefixed by an optional message</description>
   --<parameter name="Spectrum">The spectrum to be printed</parameter>
   --<parameter name="Msg">The message to be printed</parameter>
   --<exception>None at this moment</exception>

   procedure Debug_Spectrum (Spectrum : in RGB_Spectrum; Level : in Integer := 0);
   --<summary>Prints some debugging info of a Spectrum</summary>
   --<description>Prints the value of a Spectrum with the Recursion Level</description>
   --<parameter name="Spectrum">The spectrum to be printed</parameter>
   --<parameter name="Level">The Level to compare with the DEBUG_LEVEL</parameter>
   --<exception>None at this moment</exception>

   procedure Debug_Spectrum (Spectrum : in XYZ_Spectrum; Level : in Integer := 0);
   --<summary>Prints some debugging info of a Spectrum</summary>
   --<description>Prints the value of a Spectrum with the Recursion Level</description>
   --<parameter name="Spectrum">The spectrum to be printed</parameter>
   --<parameter name="Level">The Level to compare with the DEBUG_LEVEL</parameter>
   --<exception>None at this moment</exception>

   procedure Debug_Spectrum (Spectrum : in xyY_Spectrum; Level : in Integer := 0);
   --<summary>Prints some debugging info of a Spectrum</summary>
   --<description>Prints the value of a Spectrum with the Recursion Level</description>
   --<parameter name="Spectrum">The spectrum to be printed</parameter>
   --<parameter name="Level">The Level to compare with the DEBUG_LEVEL</parameter>
   --<exception>None at this moment</exception>

   function "+" (S1 : in RGB_Spectrum; S2 : in RGB_Spectrum) return RGB_Spectrum;
   --<summary>Adds two RGB_Spectra</summary>
   --<description>function to add two RGB_Spectra</description>
   --<parameter name="S1">The first spectrum</parameter>
   --<parameter name="S2">The second spectrum</parameter>
   --<exception>None at this moment</exception>

   function "*" (S1 : in RGB_Spectrum; S2 : in RGB_Spectrum) return RGB_Spectrum;
   --<summary>Multiplies two RGB_Spectra</summary>
   --<description>function to multiply two RGB_Spectra</description>
   --<parameter name="S1">The first spectrum</parameter>
   --<parameter name="S2">The second spectrum</parameter>
   --<exception>None at this moment</exception>

   function "*" (Spc : in RGB_Spectrum; S : in Small_Float) return RGB_Spectrum;
   --<summary>Multiplies an RGB_Spectrum with a scalar</summary>
   --<description>Multiplies an RGB_Spectrum with a scalar/description>
   --<parameter name="Spc">The first spectrum</parameter>
   --<parameter name="S">The scalar</parameter>
   --<exception>None at this moment</exception>

   function "*" (S : in Small_Float; Spc : in RGB_Spectrum) return RGB_Spectrum;
   --<summary>Multiplies an RGB_Spectrum with a scalar</summary>
   --<description>fMultiplies an RGB_Spectrum with a scalar</description>
   --<parameter name="S">The second spectrum</parameter>
   --<parameter name="Spc">The first spectrum</parameter>
   --<exception>None at this moment</exception>

   function "/" (Spc : in RGB_Spectrum; S : in Small_Float) return RGB_Spectrum;
   --<summary>Divides an RGB_Spectrum with a scalar</summary>
   --<description>Divides an RGB_Spectrum with a scalar/description>
   --<parameter name="Spc">The spectrum</parameter>
   --<parameter name="S">The scalar</parameter>
   --<exception>None at this moment</exception>

   function "/" (S : in Small_Float; Spc : in RGB_Spectrum) return RGB_Spectrum;
   --<summary>Divides an RGB_Spectrum with a scalar</summary>
   --<description>Divides an RGB_Spectrum with a scalar</description>
   --<parameter name="S">The spectrum</parameter>
   --<parameter name="Spc">The first spectrum</parameter>
   --<exception>None at this moment</exception>

   function Gamma_Correct (Spc : in RGB_Spectrum; G : in Small_Float) return RGB_Spectrum;
   --<summary>Gamma-corrects an RGB_Spectrum with a scalar</summary>
   --<description>Gamma-corrects an RGB_Spectrum with a scalar</description>
   --<parameter name="Spc">The first spectrum</parameter>
   --<parameter name="G">The scalar</parameter>
   --<exception>None at this moment</exception>

   function Normalize (Spc : in RGB_Spectrum) return RGB_Spectrum;
   --<summary>Normalizes an RGB_Spectrum</summary>
   --<description>Normalizes an RGB_Spectrum</description>
   --<parameter name="Spc">The first spectrum</parameter>
   --<exception>None at this moment</exception>

   function Luminance (Spc : in RGB_Spectrum) return Small_Float;
   --<summary>Computes the luminance of an RGB_Spectrum</summary>
   --<description>Computes the luminance of an RGB_Spectrum</description>
   --<exception>None at this moment</exception>

   function Get_R (Spc : in RGB_Spectrum) return Small_Float;
   --<summary>Gets the R value of an RGB_Spectrum</summary>
   --<description>Gets the R value of an RGB_Spectrum</description>
   --<exception>None at this moment</exception>

   function Get_G (Spc : in RGB_Spectrum) return Small_Float;
   --<summary>Gets the G value of an RGB_Spectrum</summary>
   --<description>Gets the G value of an RGB_Spectrum</description>
   --<exception>None at this moment</exception>

   function Get_B (Spc : in RGB_Spectrum) return Small_Float;
   --<summary>Gets the B value of an RGB_Spectrum</summary>
   --<description>Gets the B value of an RGB_Spectrum</description>
   --<exception>None at this moment</exception>

   function Get_X (Spc : in XYZ_Spectrum) return Small_Float;
   --<summary>Gets the X value of an XYZ_Spectrum</summary>
   --<description>Gets the X value of an XYZ_Spectrum</description>
   --<exception>None at this moment</exception>

   function Get_Y (Spc : in XYZ_Spectrum) return Small_Float;
   --<summary>Gets the Y value of an XYZ_Spectrum</summary>
   --<description>Gets the Y value of an XYZ_Spectrum</description>
   --<exception>None at this moment</exception>

   function Get_Z (Spc : in XYZ_Spectrum) return Small_Float;
   --<summary>Gets the Z value of an XYZ_Spectrum</summary>
   --<description>Gets the Z value of an XYZ_Spectrum</description>
   --<exception>None at this moment</exception>

   function Get_x (Spc : in xyY_Spectrum) return Small_Float;
   --<summary>Gets the x value of an xyY_Spectrum</summary>
   --<description>Gets the x value of an xyY_Spectrum</description>
   --<exception>None at this moment</exception>

   function Get_y (Spc : in xyY_Spectrum) return Small_Float;
   --<summary>Gets the y value of an xyY_Spectrum</summary>
   --<description>Gets the y value of an xyY_Spectrum</description>
   --<exception>None at this moment</exception>

   function Get_Lum (Spc : in xyY_Spectrum) return Small_Float;
   --<summary>Gets the Y (Lum) value of an xyY_Spectrum</summary>
   --<description>Gets the Y (Lum) value of an xyY_Spectrum</description>
   --<exception>None at this moment</exception>

   procedure Set_Lum (Spc : in out xyY_Spectrum; Lum : Small_Float);
   --<summary>Sets the Y (Lum) value of an xyY_Spectrum</summary>
   --<description>Sets the Y (Lum) value of an xyY_Spectrum</description>
   --<exception>None at this moment</exception>
   ---------------------
   -- ADT RGB_PixelColor
   ---------------------
   type RGB_PixelColor is private;
   --<summary>ADT of a pixel color with R, G and B values in the range 0..255</summary>

   procedure Put (PixCol : in RGB_PixelColor; Msg : in String := "RGB_PixelColor = ");
   --<summary>Prints an RGB_PixelColor</summary>
   --<description>Prints an RGB_PixelColor to the screen in a readable format, prefixed by an optional message</description>
   --<parameter name="PixCol">The spectrum to be printed</parameter>
   --<parameter name="Msg">The message to be printed</parameter>
   --<exception>None at this moment</exception>

   function Get_R (PixCol : in RGB_PixelColor) return Integer;
   --<summary>Gets the R value of an RGB_PixelColor as an Integer</summary>
   --<description>Gets the R value of an RGB_PixelColor as an Integer</description>
   --<exception>None at this moment</exception>

   function Get_G (PixCol : in RGB_PixelColor) return Integer;
   --<summary>Gets the G value of an RGB_PixelColor as an Integer</summary>
   --<description>Gets the G value of an RGB_PixelColor as an Integer</description>
   --<exception>None at this moment</exception>

   function Get_B (PixCol : in RGB_PixelColor) return Integer;
   --<summary>Gets the B value of an RGB_PixelColor as an Integer</summary>
   --<description>Gets the B value of an RGB_PixelColor as an Integer</description>
   --<exception>None at this moment</exception>

   function Convert_RGB_Spectrum (Spectrum : in RGB_Spectrum) return RGB_PixelColor;
   --<summary>Converts an RGB_Spectrum to an RGB_PixelColor</summary>
   --<description>Converts an RGB_Spectrum to an RGB_PixelColor</description>
   --<parameter name="Spectrum">The RGB_Spectrum to convert</parameter>
   --<exception>None at this moment</exception>

   function Convert_RGB_Spectrum (Spectrum : in RGB_Spectrum) return XYZ_Spectrum;
   --<summary>Converts an RGB_Spectrum to an XYZ_Spectrum</summary>
   --<description>Converts an RGB_Spectrum to an XYZ_Spectrum</description>
   --<parameter name="Spectrum">The RGB_Spectrum to convert</parameter>
   --<exception>None at this moment</exception>

   function Convert_XYZ_Spectrum (Spectrum : in XYZ_Spectrum) return xyY_Spectrum;
   --<summary>Converts an XYZ_Spectrum to an xyY_Spectrum</summary>
   --<description>Converts an XYZ_Spectrum to an xyY_Spectrum</description>
   --<parameter name="Spectrum">The XYZ_Spectrum to convert</parameter>
   --<exception>None at this moment</exception>

   function Convert_XYZ_Spectrum (Spectrum : in XYZ_Spectrum) return RGB_Spectrum;
   --<summary>Converts an XYZ_Spectrum to an RGB_Spectrum</summary>
   --<description>Converts an XYZ_Spectrum to an RGB_Spectrum</description>
   --<parameter name="Spectrum">The XYZ_Spectrum to convert</parameter>
   --<exception>None at this moment</exception>

   function Convert_xyY_Spectrum (Spectrum : in xyY_Spectrum) return XYZ_Spectrum;
   --<summary>Converts an xyY_Spectrum to an XYZ_Spectrum</summary>
   --<description>Converts an xyY_Spectrum to an XYZ_Spectrum</description>
   --<parameter name="Spectrum">The xyY_Spectrum to convert</parameter>
   --<exception>None at this moment</exception>

   BLACK_RGB_Spec      : constant RGB_Spectrum;
   WHITE_RGB_Spec      : constant RGB_Spectrum;
   RED_RGB_Spec        : constant RGB_Spectrum;
   GREEN_RGB_Spec      : constant RGB_Spectrum;
   BLUE_RGB_Spec       : constant RGB_Spectrum;
   YELLOW_RGB_Spec     : constant RGB_Spectrum;
   PURPLE_RGB_Spec     : constant RGB_Spectrum;
   CYAN_RGB_Spec       : constant RGB_Spectrum;
   LIGHT_GREY_RGB_Spec : constant RGB_Spectrum;
   MID_GREY_RGB_Spec   : constant RGB_Spectrum;
   DARK_GREY_RGB_Spec  : constant RGB_Spectrum;

   -- CIE Matrix values to convert RGB to XYZ
   -- See http://www.brucelindbloom.com/index.html?Eqn_RGB_to_XYZ.html
   RGB2XYZ_M11 : constant Small_Float := 0.488_718_0;
   RGB2XYZ_M12 : constant Small_Float := 0.310_680_3;
   RGB2XYZ_M13 : constant Small_Float := 0.200_601_7;
   RGB2XYZ_M21 : constant Small_Float := 0.176_204_4;
   RGB2XYZ_M22 : constant Small_Float := 0.812_984_7;
   RGB2XYZ_M23 : constant Small_Float := 0.010_810_9;
   RGB2XYZ_M31 : constant Small_Float := 0.000_000_0;
   RGB2XYZ_M32 : constant Small_Float := 0.010_204_8;
   RGB2XYZ_M33 : constant Small_Float := 0.989_795_2;

   -- CIE Matrix values to convert XYZ to RGB (Inverse of the above
   -- See http://www.brucelindbloom.com/index.html?Eqn_RGB_to_XYZ.html
   XYZ2RGB_M11 : constant Small_Float := 2.370_674_3;
   XYZ2RGB_M12 : constant Small_Float := -0.900_040_5;
   XYZ2RGB_M13 : constant Small_Float := -0.470_633_8;
   XYZ2RGB_M21 : constant Small_Float := -0.513_885_0;
   XYZ2RGB_M22 : constant Small_Float := 1.425_303_6;
   XYZ2RGB_M23 : constant Small_Float := 0.088_581_4;
   XYZ2RGB_M31 : constant Small_Float := 0.005_298_2;
   XYZ2RGB_M32 : constant Small_Float := -0.014_694_9;
   XYZ2RGB_M33 : constant Small_Float := 1.009_396_8;

   -- factors used to compute luminance from RGB values. When all added, should equal 1
   -- RED_FACTOR   : constant Small_Float := 0.299;
   -- GREEN_FACTOR : constant Small_Float := 0.587;
   -- BLUE_FACTOR  : constant Small_Float := 0.114;

   -- CIE RGB factors: 0.1762044  0.8129847  0.0108109
   RED_FACTOR   : constant Small_Float := 0.176_204_4;
   GREEN_FACTOR : constant Small_Float := 0.812_984_7;
   BLUE_FACTOR  : constant Small_Float := 0.010_810_9;

private

   type RGB_Spectrum is record
      R, G, B : Small_Float := 0.0;
   end record;

   type XYZ_Spectrum is record
      X, Y, Z : Small_Float := 0.0;
   end record;

   type xyY_Spectrum is record
      x, y, Lum : Small_Float := 0.0;
   end record;

   type RGB_Value is range 0 .. 255;

   type RGB_PixelColor is record
      R, G, B : RGB_Value;
   end record;

   BLACK_RGB_Spec      : constant RGB_Spectrum := (R => 0.0, G => 0.0, B => 0.0);
   WHITE_RGB_Spec      : constant RGB_Spectrum := (R => 1.0, G => 1.0, B => 1.0);
   RED_RGB_Spec        : constant RGB_Spectrum := (R => 1.0, G => 0.0, B => 0.0);
   GREEN_RGB_Spec      : constant RGB_Spectrum := (R => 0.0, G => 1.0, B => 0.0);
   BLUE_RGB_Spec       : constant RGB_Spectrum := (R => 0.0, G => 0.0, B => 1.0);
   YELLOW_RGB_Spec     : constant RGB_Spectrum := (R => 1.0, G => 1.0, B => 0.0);
   PURPLE_RGB_Spec     : constant RGB_Spectrum := (R => 1.0, G => 0.0, B => 1.0);
   CYAN_RGB_Spec       : constant RGB_Spectrum := (R => 0.0, G => 1.0, B => 1.0);
   LIGHT_GREY_RGB_Spec : constant RGB_Spectrum := (R => 0.75, G => 0.75, B => 0.75);
   MID_GREY_RGB_Spec   : constant RGB_Spectrum := (R => 0.5, G => 0.5, B => 0.5);
   DARK_GREY_RGB_Spec  : constant RGB_Spectrum := (R => 0.25, G => 0.25, B => 0.25);

end Spectra;
