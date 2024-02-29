with Utilities;           use Utilities;
with Scenes;              use Scenes;
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
--  with Small_Float_Functions; use Small_Float_Functions; with Normal_Float_Functions; use Normal_Float_Functions; with
--  Large_Float_Functions; use Large_Float_Functions;

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

   procedure Debug_Spectrum (Spectrum : in RGB_Spectrum; Level : in Integer := 0) is
      Rec : Natural;
   begin
      if Level <= DEBUG_LEVEL then
         Rec := Get_Recursion_Level;
         Put_Line ("RECUSRION_DEPTH = " & Rec'Image);
         Put_Line ("The following spectrum is returned:");
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

   function Get_R (Spc : in RGB_Spectrum) return Small_Float is (Spc.R);

   function Get_G (Spc : in RGB_Spectrum) return Small_Float is (Spc.G);

   function Get_B (Spc : in RGB_Spectrum) return Small_Float is (Spc.B);

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
      PixCol   : RGB_PixelColor;
      Temp_Col : Integer;
   begin
      --  Compute R and clip in 0..255 range
      Temp_Col := Integer (Small_Float'Floor (0.5 + Spectrum.R * 255.0));
      if Temp_Col < 0 then
         Debug_Message ("CLIPPED RED TO 0: " & Temp_Col'Image, 2);
         Temp_Col := 0;
      elsif Temp_Col > 255 then
         Debug_Message ("CLIPPED RED TO 255: " & Temp_Col'Image, 2);
         Debug_Message ("RED RAD   : " & Spectrum.R'Image, 2);
         Debug_Message ("GREEN RAD : " & Spectrum.G'Image, 2);
         Debug_Message ("BLUE RAD  : " & Spectrum.B'Image, 2);
         Temp_Col := 255;
      end if;
      PixCol.R := RGB_Value (Temp_Col);

      --  Compute G and clip in 0..255 range
      Temp_Col := Integer (Small_Float'Floor (0.5 + Spectrum.G * 255.0));
      if Temp_Col < 0 then
         Debug_Message ("CLIPPED GREEN TO 0: " & Temp_Col'Image, 2);
         Temp_Col := 0;
      elsif Temp_Col > 255 then
         Debug_Message ("CLIPPED GREEN TO 255: " & Temp_Col'Image, 2);
         Debug_Message ("RED RAD   : " & Spectrum.R'Image, 2);
         Debug_Message ("GREEN RAD : " & Spectrum.G'Image, 2);
         Debug_Message ("BLUE RAD  : " & Spectrum.B'Image, 2);
         Temp_Col := 255;
      end if;
      PixCol.G := RGB_Value (Temp_Col);

      --  Compute B and clip in 0..255 range
      Temp_Col := Integer (Small_Float'Floor (0.5 + Spectrum.B * 255.0));
      if Temp_Col < 0 then
         Debug_Message ("CLIPPED BLUE TO 0: " & Temp_Col'Image, 2);
         Temp_Col := 0;
      elsif Temp_Col > 255 then
         Debug_Message ("CLIPPED BLUE TO 255: " & Temp_Col'Image, 2);
         Debug_Message ("RED RAD   : " & Spectrum.R'Image, 2);
         Debug_Message ("GREEN RAD : " & Spectrum.G'Image, 2);
         Debug_Message ("BLUE RAD  : " & Spectrum.B'Image, 2);
         Temp_Col := 255;
      end if;
      PixCol.B := RGB_Value (Temp_Col);

      return PixCol;
   end Convert_RGB_Spectrum;

   function Get_R (PixCol : in RGB_PixelColor) return Integer is (Integer (PixCol.R));

   function Get_G (PixCol : in RGB_PixelColor) return Integer is (Integer (PixCol.G));

   function Get_B (PixCol : in RGB_PixelColor) return Integer is (Integer (PixCol.B));

end Spectra;
