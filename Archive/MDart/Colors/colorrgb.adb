--  Package ColorRGB contains the basic types and functions to handle RGB Color Space

with Ada.Text_IO; use Ada.Text_IO;

package body ColorRGB is

   --  Print a Color_RGB
   procedure Put (Color : in Color_RGB) is
   begin
      Put ("Color_RGB = { ");
      Put (Color.R);
      Put (" , ");
      Put (Color.G);
      Put (" , ");
      Put (Color.B);
      Put_Line (" }");
   end Put;

   --  Construct a Color_RGB from three floats
   function ConstructColor_RGB (R, G, B : in SMALL_FLOAT) return Color_RGB is
      Result : Color_RGB;
   begin
      Result.R := R;
      Result.G := G;
      Result.B := B;
      return Result;
   end ConstructColor_RGB;

   --  Function to add two Color_RGB variables
   function "+" (Left, Right : in Color_RGB) return Color_RGB is
      Result : Color_RGB;
   begin
      Result.R := Left.R + Right.R;
      Result.G := Left.G + Right.G;
      Result.B := Left.B + Right.B;
      return Result;
   end "+";

   --  Function to multiply two Color_RGB variables
   function "*" (Left, Right : in Color_RGB) return Color_RGB is
      Result : Color_RGB;
   begin
      Result.R := Left.R * Right.R;
      Result.G := Left.G * Right.G;
      Result.B := Left.B * Right.B;
      return Result;
   end "*";

   --  Function to convert a Color_RGB to an X11 Integer Color
   function Color_RGB_To_X11 (Color : in Color_RGB) return Integer is
      Result : Integer;
   begin
      if Color.R < 0.0 or Color.G < 0.0 or Color.B < 0.0 then
         Put_Line ("*** Color Underflow Error in Color_RGB_To_X11");
         return 65536 * 255 + 256 * 255 + 255;
      end if;
      if Color.R > 1.0 or Color.G > 1.0 or Color.B > 1.0 then
         Put_Line ("*** Color Overflow Error in Color_RGB_To_X11");
         return 65536 * 255 + 256 * 255 + 255;
      end if;
      Result := 65536 * Integer (Color.R * 255.0) +
                256 * Integer (Color.G * 255.0) +
                Integer (Color.B * 255.0);
      return Result;
   end Color_RGB_To_X11;

end ColorRGB;
