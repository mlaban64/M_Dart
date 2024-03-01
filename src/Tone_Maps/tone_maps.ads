with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Core_Types;            use Core_Types;
with Spectra;               use Spectra;

--<summary>The Tone_Maps package provides routines for tone mapping operations</summary>
--<description>The Tone_Maps package provides routines for tone mapping operations. The Tone_Map is the main structure to
--hold an image as a set of RGB_Spectra and Luminance values. After the image has been built up on screen, the Tone_Map is used
--to recompute the image by some tone mapping algorithm</description>
--<group>Utilities</group>
package Tone_Maps is

   type Tone_Map (X_Max, Y_Max : Integer) is private;
   --<summary>Tone_Map represents a frame buffer to store images as radiance & luminance values</summary>

   Main_Tone_Map : access Tone_Map;
   --  the main tone map buffer pointer

   procedure Create_Tone_Map (XRes, YRes : in Integer);
   --<summary>Create the Main_Tone_Map with a specific resolution</summary>
   --<description>Create the Main_Tone_Map with a specific resolution</description>
   --<parameter name="XRes">The width of the window to be created</parameter>
   --<parameter name="YRes">The height of the window to be created</parameter>
   --<exception>None at this moment</exception>

   function Refactor_Radiance (Radiance : in RGB_Spectrum) return RGB_Spectrum;
   --<summary>Divides a Spectrum by the Luminance Factors per color</summary>
   --<description>Divides a Spectrum by the Luminance Factors per color</description>
   --<parameter name="Radiance">The spectrum to divide</parameter>
   --<exception>None at this moment</exception>

   procedure Set_Pixel (X, Y : in Integer; Radiance : in RGB_Spectrum);
   --<summary>Sets a pixel to a radiance, and compute the luminance and store it as well</summary>
   --<description>Sets a pixel to a radiance, and compute the luminance and store it as well</description>
   --<parameter name="X">The X coordinate of the pixel to be set</parameter>
   --<parameter name="Y">The Y coordinate of the pixel to be set</parameter>
   --<parameter name="Radiance">The RGB_Spectrum of the color to set the pixel with</parameter>
   --<exception>None at this moment</exception>

   procedure Map_Tones_To_Image_Linear;
   --<summary>Maps the Tone Map image to the screen and image buffer</summary>
   --<description>Maps the Tone Map image to the screen and image buffer by a simple division of all RGB values
   -- by the maximum R, G or B value of the whole image</description>
   --<exception>None at this moment</exception>

   procedure Map_Tones_To_Image_Reinhard;
   --<summary>Maps the Tone Map image to the screen and image buffer</summary>
   --<description>Maps the Tone Map image to the screen and image buffer by the Reinhard formula Lo = Li / (1.0 + Li)</description>
   --<exception>None at this moment</exception>

   procedure Map_Tones_To_Image_Reinhard_Extended;
   --<summary>Maps the Tone Map image to the screen and image buffer</summary>
   --<description>Maps the Tone Map image to the screen and image buffer by the Reinhard Extended
   -- formula Lo = Li * (1 + Li/Lmax^2 )/ (1.0 + Li)</description>
   --<exception>None at this moment</exception>

private

   --  factors used to compute luminance from RGB values. When all added, should equal 1
   -- RED_FACTOR   : constant Small_Float := 0.299;
   -- GREEN_FACTOR : constant Small_Float := 0.587;
   -- BLUE_FACTOR  : constant Small_Float := 0.114;

   -- CIE RGB factors: 0.1762044  0.8129847  0.0108109
   RED_FACTOR   : constant Small_Float := 0.176_204_4;
   GREEN_FACTOR : constant Small_Float := 0.812_984_7;
   BLUE_FACTOR  : constant Small_Float := 0.010_810_9;

   type Tone_Pixel is record
      Radiance  : RGB_Spectrum;
      Luminance : Small_Float := 0.0;
   end record;

   type Tone_Buffer is array (Integer range <>, Integer range <>) of Tone_Pixel;
   --  <summary>Tone_Buffer is a generic array to instantiate a tone buffer</summary>

   type Tone_Map (X_Max, Y_Max : Integer) is record
      ToneBuffer : Tone_Buffer (0 .. X_Max, 0 .. Y_Max);
      XRes, YRes : Integer;
      Imin, Imax : Small_Float;
      --  Min & Max luminance values for the image
      Xmin, Ymin : Integer;
      --  Coordinate belonging to the min luminance
      Xmax, Ymax : Integer;
      --  Coordinate belonging to the max luminance
      Rmax, Gmax, Bmax : Small_Float;
      --  Overall max R, G and B value
      Rmin, Gmin, Bmin : Small_Float;
      --  Overall min R, G and B value
   end record;

end Tone_Maps;
