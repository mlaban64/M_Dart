with Utilities; use Utilities;
with Scenes; use Scenes;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with Small_Float_Functions; use Small_Float_Functions;
-- with Normal_Float_Functions; use Normal_Float_Functions;
-- with Large_Float_Functions;  use Large_Float_Functions;

package body Spectra is

   -------------------
   -- ADT RGB_Spectrum
   -------------------
   function Construct_RGB_Spectrum (R, G, B : in Small_Float) return RGB_Spectrum is
      Spectrum : RGB_Spectrum;
   begin
      Spectrum.R := R;
      Spectrum.G := G;
      Spectrum.B := B;
      return Spectrum;
   end Construct_RGB_Spectrum;

   procedure Put (Spectrum : in RGB_Spectrum; Msg : in String := "RGB_Spectrum = ") is
   begin
      Put (Msg);
      New_Line;
      Put ("   {");
      Put (Spectrum.R);
      Put (" ,");
      Put (Spectrum.G);
      Put (" ,");
      Put (Spectrum.B);
      Put (" }");
      New_Line;
   end Put;

   procedure Put (Spectrum : in XYZ_Spectrum; Msg : in String := "XYZ_Spectrum = ") is
   begin
      Put (Msg);
      New_Line;
      Put ("   {");
      Put (Spectrum.X);
      Put (" ,");
      Put (Spectrum.Y);
      Put (" ,");
      Put (Spectrum.Z);
      Put (" }");
      New_Line;
   end Put;

   procedure Put (Spectrum : in xyY_Spectrum; Msg : in String := "xyY_Spectrum = ") is
   begin
      Put (Msg);
      New_Line;
      Put ("   {");
      Put (Spectrum.x);
      Put (" ,");
      Put (Spectrum.y);
      Put (" ,");
      Put (Spectrum.Lum);
      Put (" }");
      New_Line;
   end Put;

   procedure Debug_Spectrum (Spectrum : in RGB_Spectrum; Level : in Integer := 0) is
      Rec : Natural;
   begin
      if Level <= DEBUG_LEVEL then
         Rec := Get_Recursion_Level;
         Put_Line ("RECUSRION_DEPTH = " & Rec'Image);
         Put_Line ("The following RGB spectrum is returned:");
         Put (Spectrum);
      end if;
   end Debug_Spectrum;

   procedure Debug_Spectrum (Spectrum : in XYZ_Spectrum; Level : in Integer := 0) is
      Rec : Natural;
   begin
      if Level <= DEBUG_LEVEL then
         Rec := Get_Recursion_Level;
         Put_Line ("RECUSRION_DEPTH = " & Rec'Image);
         Put_Line ("The following XYZ spectrum is returned:");
         Put (Spectrum);
      end if;
   end Debug_Spectrum;

   procedure Debug_Spectrum (Spectrum : in xyY_Spectrum; Level : in Integer := 0) is
      Rec : Natural;
   begin
      if Level <= DEBUG_LEVEL then
         Rec := Get_Recursion_Level;
         Put_Line ("RECUSRION_DEPTH = " & Rec'Image);
         Put_Line ("The following xyY spectrum is returned:");
         Put (Spectrum);
      end if;
   end Debug_Spectrum;

   function "+" (S1 : in RGB_Spectrum; S2 : in RGB_Spectrum) return RGB_Spectrum is
   begin
      return New_Spc : RGB_Spectrum do
         New_Spc.R := S1.R + S2.R;
         New_Spc.G := S1.G + S2.G;
         New_Spc.B := S1.B + S2.B;
      end return;
   end "+";

   function "*" (S1 : in RGB_Spectrum; S2 : in RGB_Spectrum) return RGB_Spectrum is
   begin
      return New_Spc : RGB_Spectrum do
         New_Spc.R := S1.R * S2.R;
         New_Spc.G := S1.G * S2.G;
         New_Spc.B := S1.B * S2.B;
      end return;
   end "*";

   function "*" (Spc : in RGB_Spectrum; S : in Small_Float) return RGB_Spectrum is
   begin
      return New_Spc : RGB_Spectrum do
         New_Spc.R := Spc.R * S;
         New_Spc.G := Spc.G * S;
         New_Spc.B := Spc.B * S;
      end return;
   end "*";

   function "*" (S : in Small_Float; Spc : in RGB_Spectrum) return RGB_Spectrum is
   begin
      return New_Spc : RGB_Spectrum do
         New_Spc.R := Spc.R * S;
         New_Spc.G := Spc.G * S;
         New_Spc.B := Spc.B * S;
      end return;
   end "*";

   function "/" (Spc : in RGB_Spectrum; S : in Small_Float) return RGB_Spectrum is
   begin
      return New_Spc : RGB_Spectrum do
         New_Spc.R := Spc.R / S;
         New_Spc.G := Spc.G / S;
         New_Spc.B := Spc.B / S;
      end return;
   end "/";

   function "/" (S : in Small_Float; Spc : in RGB_Spectrum) return RGB_Spectrum is
   begin
      return New_Spc : RGB_Spectrum do
         New_Spc.R := Spc.R / S;
         New_Spc.G := Spc.G / S;
         New_Spc.B := Spc.B / S;
      end return;
   end "/";

   function Gamma_Correct (Spc : in RGB_Spectrum; G : in Small_Float) return RGB_Spectrum is
      Pow : Small_Float;
      New_Spc : RGB_Spectrum;
   begin
      Pow := 1.0 / G;

      -- Test for RGB > 0, as sometimes it is 0 or very small negative. This raises an ARG error with the ** operator
      -- As New_Spc is initialized to 0.0, we only correct positive values to ensure no negative final RGB spectrum
      if Spc.R > 0.0 then
         New_Spc.R := Spc.R**Pow;
      end if;
      if Spc.G > 0.0 then
         New_Spc.G := Spc.G**Pow;
      end if;
      if Spc.B > 0.0 then
         New_Spc.B := Spc.B**Pow;
      end if;

      return New_Spc;
   end Gamma_Correct;

   function Normalize (Spc : in RGB_Spectrum) return RGB_Spectrum is
      Len : Small_Float;
      New_Spc : RGB_Spectrum;
   begin
      Len := Sqrt (Spc.R * Spc.R + Spc.G * Spc.G + Spc.B * Spc.B);
      New_Spc.R := Spc.R / Len;
      New_Spc.G := Spc.G / Len;
      New_Spc.B := Spc.B / Len;

      return New_Spc;
   end Normalize;

   function Luminance (Spc : in RGB_Spectrum) return Small_Float is
   begin
      return (RED_FACTOR * Spc.R + GREEN_FACTOR * Spc.G + BLUE_FACTOR * Spc.B);
   end Luminance;

   function Get_R (Spc : in RGB_Spectrum) return Small_Float is (Spc.R);

   function Get_G (Spc : in RGB_Spectrum) return Small_Float is (Spc.G);

   function Get_B (Spc : in RGB_Spectrum) return Small_Float is (Spc.B);

   function Get_X (Spc : in XYZ_Spectrum) return Small_Float is (Spc.X);

   function Get_Y (Spc : in XYZ_Spectrum) return Small_Float is (Spc.Y);

   function Get_Z (Spc : in XYZ_Spectrum) return Small_Float is (Spc.Z);

   function Get_x (Spc : in xyY_Spectrum) return Small_Float is (Spc.x);

   function Get_y (Spc : in xyY_Spectrum) return Small_Float is (Spc.y);

   function Get_Lum (Spc : in xyY_Spectrum) return Small_Float is (Spc.Lum);

   procedure Set_Lum (Spc : in out xyY_Spectrum; Lum : Small_Float) is
   begin
      Spc.Lum := Lum;
   end Set_Lum;

   ---------------------
   -- ADT RGB_PixelColor
   ---------------------
   procedure Put (PixCol : in RGB_PixelColor; Msg : in String := "RGB_PixelColor = ") is
   begin
      Put (Msg);
      New_Line;
      Put ("   {");
      Put (Natural (PixCol.R), 4);
      Put (" ,");
      Put (Natural (PixCol.G), 4);
      Put (" ,");
      Put (Natural (PixCol.B), 4);
      Put (" }");
      New_Line;
   end Put;

   function Convert_RGB_Spectrum (Spectrum : in RGB_Spectrum) return RGB_PixelColor is
      PixCol : RGB_PixelColor;
      Temp_Col : Integer;
   begin
      --  Compute R and clip in 0..255 range
      Temp_Col := Integer (Small_Float'Floor (0.5 + Spectrum.R * 255.0));
      if Temp_Col < 0 then
         Debug_Message ("CLIPPED RED TO 0: " & Spectrum.R'Image, 2);
         Temp_Col := 0;
      elsif Temp_Col > 255 then
         Debug_Message ("CLIPPED RED TO 255: " & Spectrum.R'Image, 2);
         Temp_Col := 255;
      end if;
      PixCol.R := RGB_Value (Temp_Col);

      --  Compute G and clip in 0..255 range
      Temp_Col := Integer (Small_Float'Floor (0.5 + Spectrum.G * 255.0));
      if Temp_Col < 0 then
         Debug_Message ("CLIPPED GREEN TO 0: " & Spectrum.G'Image, 2);
         Temp_Col := 0;
      elsif Temp_Col > 255 then
         Debug_Message ("CLIPPED GREEN TO 255: " & Spectrum.G'Image, 2);
         Temp_Col := 255;
      end if;
      PixCol.G := RGB_Value (Temp_Col);

      --  Compute B and clip in 0..255 range
      Temp_Col := Integer (Small_Float'Floor (0.5 + Spectrum.B * 255.0));
      if Temp_Col < 0 then
         Debug_Message ("CLIPPED BLUE TO 0: " & Spectrum.B'Image, 2);
         Temp_Col := 0;
      elsif Temp_Col > 255 then
         Debug_Message ("CLIPPED BLUE TO 255: " & Spectrum.B'Image, 2);
         Temp_Col := 255;
      end if;
      PixCol.B := RGB_Value (Temp_Col);

      return PixCol;
   end Convert_RGB_Spectrum;

   function Convert_RGB_Spectrum (Spectrum : in RGB_Spectrum) return XYZ_Spectrum is
      XYZ : XYZ_Spectrum;
   begin
      -- Assuming RGB in is already linear to energy, else see http://www.brucelindbloom.com/index.html?Eqn_RGB_to_XYZ.html
      -- Usng CIE RGB matrix from same URL
      XYZ.X := RGB2XYZ_M11 * Spectrum.R + RGB2XYZ_M12 * Spectrum.G + RGB2XYZ_M13 * Spectrum.B;
      XYZ.Y := RGB2XYZ_M21 * Spectrum.R + RGB2XYZ_M22 * Spectrum.G + RGB2XYZ_M23 * Spectrum.B;
      XYZ.Z := RGB2XYZ_M31 * Spectrum.R + RGB2XYZ_M32 * Spectrum.G + RGB2XYZ_M33 * Spectrum.B;

      return XYZ;
   end Convert_RGB_Spectrum;

   function Convert_XYZ_Spectrum (Spectrum : in XYZ_Spectrum) return xyY_Spectrum is
      xyY : xyY_Spectrum;
      XYZ : Small_Float;
   begin
      XYZ := Spectrum.X + Spectrum.Y + Spectrum.Z;
      xyY.x := Spectrum.X / XYZ;
      xyY.y := Spectrum.Y / XYZ;
      xyY.Lum := Spectrum.Y;
      return xyY;
   end Convert_XYZ_Spectrum;

   function Convert_XYZ_Spectrum (Spectrum : in XYZ_Spectrum) return RGB_Spectrum is
      RGB : RGB_Spectrum;
   begin
      -- See http://www.brucelindbloom.com/index.html?Eqn_RGB_to_XYZ.html
      -- Usng CIE RGB matrix M-1 from same URL
      RGB.R := XYZ2RGB_M11 * Spectrum.X + XYZ2RGB_M12 * Spectrum.Y + XYZ2RGB_M13 * Spectrum.Z;
      RGB.G := XYZ2RGB_M21 * Spectrum.X + XYZ2RGB_M22 * Spectrum.Y + XYZ2RGB_M23 * Spectrum.Z;
      RGB.B := XYZ2RGB_M31 * Spectrum.X + XYZ2RGB_M32 * Spectrum.Y + XYZ2RGB_M33 * Spectrum.Z;

      return RGB;
   end Convert_XYZ_Spectrum;

   function Convert_xyY_Spectrum (Spectrum : in xyY_Spectrum) return XYZ_Spectrum is
      XYZ : XYZ_Spectrum;
   begin
      if Spectrum.y > 0.0 then
         XYZ.X := Spectrum.x * Spectrum.Lum / Spectrum.y;
         XYZ.Y := Spectrum.Lum;
         XYZ.Z := (1.0 - Spectrum.x - Spectrum.y) * Spectrum.Lum / Spectrum.y;
      else
         XYZ.X := 0.0;
         XYZ.Y := 0.0;
         XYZ.Z := 0.0;
      end if;

      return XYZ;
   end Convert_xyY_Spectrum;

   function Get_R (PixCol : in RGB_PixelColor) return Integer is (Integer (PixCol.R));

   function Get_G (PixCol : in RGB_PixelColor) return Integer is (Integer (PixCol.G));

   function Get_B (PixCol : in RGB_PixelColor) return Integer is (Integer (PixCol.B));

end Spectra;
