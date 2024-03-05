with M_GraphiX;           use M_GraphiX;
with Utilities;           use Utilities;
with Spectra;             use Spectra;
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Tone_Maps is

   procedure Create_Tone_Map (XRes, YRes : in Integer) is
   begin
      Main_Tone_Map      := new Tone_Map (XRes - 1, YRes - 1);
      Main_Tone_Map.XRes := XRes;
      Main_Tone_Map.YRes := YRes;
   end Create_Tone_Map;

   procedure Set_Pixel (X, Y : in Integer; Radiance : in RGB_Spectrum) is
      Lum, R, G, B : Small_Float;
   begin
      --  Initialize
      R := Get_R (Radiance);
      G := Get_G (Radiance);
      B := Get_B (Radiance);

      -- Some debug info
      if R < 0.0 then
         Debug_Message ("Set_Pixel: RED < 0.0", 2);
      end if;
      if G < 0.0 then
         Debug_Message ("Set_Pixel: GREEN < 0.0", 2);
      end if;
      if B < 0.0 then
         Debug_Message ("Set_Pixel: BLUE < 0.0", 2);
      end if;

      Lum := Luminance (Radiance);

      --  Set the pixel & luminance, min & max luminance, min & max RGB values
      Main_Tone_Map.ToneBuffer (X, Y).Luminance := Lum;
      Main_Tone_Map.ToneBuffer (X, Y).Radiance  := Radiance;

      if Lum > Main_Tone_Map.Imax then
         Main_Tone_Map.Imax := Lum;
         Main_Tone_Map.Xmax := X;
         Main_Tone_Map.Ymax := Y;

      end if;

      if Lum < Main_Tone_Map.Imin then
         Main_Tone_Map.Imin := Lum;
         Main_Tone_Map.Xmin := X;
         Main_Tone_Map.Ymin := Y;
      end if;

      if R > Main_Tone_Map.Rmax then
         Main_Tone_Map.Rmax := R;
      end if;

      if R < Main_Tone_Map.Rmin then
         Main_Tone_Map.Rmin := R;
      end if;

      if G > Main_Tone_Map.Gmax then
         Main_Tone_Map.Gmax := G;
      end if;

      if G < Main_Tone_Map.Gmin then
         Main_Tone_Map.Gmin := G;
      end if;

      if B > Main_Tone_Map.Bmax then
         Main_Tone_Map.Bmax := B;
      end if;

      if B < Main_Tone_Map.Bmin then
         Main_Tone_Map.Bmin := B;
      end if;

   end Set_Pixel;

   procedure Map_Tones_To_Image_Linear is
      Col                  : RGB_PixelColor;
      RGB                  : RGB_Spectrum;
      XYZ                  : XYZ_Spectrum;
      xyY                  : xyY_Spectrum;
      L_in, L_out, L_range : Small_Float;
      max_RGB              : Small_Float;
   begin
      New_Line;
      Put_Line ("Computing Linear Tone Mapping");

      -- First, map all Tone Map radiances into the 0..1 range
      -- This is done by dividing all RGB values by the maximum RGB value found
      -- The ToneBuffer_Norm is used to store the normalized Radiance spectra, so we do not destroy the rendering values
      max_RGB := Main_Tone_Map.Rmax;
      if max_RGB < Main_Tone_Map.Gmax then
         max_RGB := Main_Tone_Map.Gmax;
      end if;
      if max_RGB < Main_Tone_Map.Bmax then
         max_RGB := Main_Tone_Map.Bmax;
      end if;
      Put_Line ("Max RGB = " & max_RGB'Image);

      Put_Line ("Computing Normalized Tone Map");
      Main_Tone_Map.INmin := 999.999;
      Main_Tone_Map.INmax := 0.0;
      -- Compute the normalized values
      for Y in 0 .. Main_Tone_Map.YRes - 1 loop
         for X in 0 .. Main_Tone_Map.XRes - 1 loop
            Main_Tone_Map.ToneBuffer_Norm (X, Y).Radiance  := Main_Tone_Map.ToneBuffer (X, Y).Radiance / max_RGB;
            Main_Tone_Map.ToneBuffer_Norm (X, Y).Luminance := Luminance (Main_Tone_Map.ToneBuffer_Norm (X, Y).Radiance);
            -- Set Max & Min Normalized Luminance
            if Main_Tone_Map.ToneBuffer_Norm (X, Y).Luminance > Main_Tone_Map.INmax then
               Main_Tone_Map.INmax := Main_Tone_Map.ToneBuffer_Norm (X, Y).Luminance;
               Main_Tone_Map.XNmax := X;
               Main_Tone_Map.YNmax := Y;
            end if;
            if Main_Tone_Map.ToneBuffer_Norm (X, Y).Luminance < Main_Tone_Map.INmin then
               Main_Tone_Map.INmin := Main_Tone_Map.ToneBuffer_Norm (X, Y).Luminance;
               Main_Tone_Map.XNmin := X;
               Main_Tone_Map.YNmin := Y;
            end if;

         end loop;
      end loop;

      -- Compute the luminance range
      L_range := Main_Tone_Map.INmax - Main_Tone_Map.INmin;
      Put_Line ("Luminance range  = " & L_range'Image);

      --  Loop through all pixels
      for Y in 0 .. Main_Tone_Map.YRes - 1 loop
         for X in 0 .. Main_Tone_Map.XRes - 1 loop

            -- Normalize the RGB spectrum
            RGB := Main_Tone_Map.ToneBuffer_Norm (X, Y).Radiance;

            -- RGB to XYZ to xyY
            XYZ := Convert_RGB_Spectrum (RGB);
            xyY := Convert_XYZ_Spectrum (XYZ);

            -- Subtract the minimum Luminance, then divide by the Luminance range
            -- so we map the Lmin-Lmax band to 0..1
            L_in  := Get_Lum (xyY);
            L_out := (L_in - Main_Tone_Map.INmin) / L_range;
            Set_Lum (xyY, L_out);

            if L_out > 1.0 then
               Put_Line ("L_out > 0 : " & L_out'Image);
               Put_Line ("L_in was : " & L_in'Image);
            end if;

            XYZ := Convert_xyY_Spectrum (xyY);
            RGB := Convert_XYZ_Spectrum (XYZ);
            RGB := Gamma_Correct (RGB, GAMMA_CORRECTION);
            Col := Convert_RGB_Spectrum (RGB);
            Set_Pixel_With_Buffer (X, Y, Get_R (Col), Get_G (Col), Get_B (Col));
         end loop;
      end loop;

      Put_Line ("DONE Computing Linear Tone Mapping");
      New_Line;

   end Map_Tones_To_Image_Linear;

   procedure Map_Tones_To_Image_Reinhard is
      Col                  : RGB_PixelColor;
      RGB                  : RGB_Spectrum;
      XYZ                  : XYZ_Spectrum;
      xyY                  : xyY_Spectrum;
      total_Lum            : Large_Float;
      avg_Lum, L_in, L_out : Small_Float;
   begin
      New_Line;
      Put_Line ("Computing Reinhard Tone Mapping");

      -- Compute total and average luminance
      Put_Line ("Computing average luminance");
      total_Lum := 0.0;
      for Y in 0 .. Main_Tone_Map.YRes - 1 loop
         for X in 0 .. Main_Tone_Map.XRes - 1 loop
            total_Lum := total_Lum + Large_Float (Main_Tone_Map.ToneBuffer (X, Y).Luminance);
         end loop;
      end loop;
      avg_Lum := Small_Float (total_Lum / Large_Float ((Main_Tone_Map.XRes * Main_Tone_Map.YRes)));
      Put_Line ("Average Luminance = " & avg_Lum'Image);

      --  Loop through all pixels
      for Y in 0 .. Main_Tone_Map.YRes - 1 loop
         for X in 0 .. Main_Tone_Map.XRes - 1 loop

            -- Normalize the RGB spectrum
            RGB := Normalize (Main_Tone_Map.ToneBuffer (X, Y).Radiance);

            -- RGB to XYZ to xyY
            XYZ := Convert_RGB_Spectrum (Main_Tone_Map.ToneBuffer (X, Y).Radiance);
            xyY := Convert_XYZ_Spectrum (XYZ);

            -- See https://bruop.github.io/tonemapping/
            -- See http://www.brucelindbloom.com/index.html?Math.html
            L_in := Get_Lum (xyY);

            -- Exposure correction or not?
            -- L_out := L_in / (9.6 * avg_Lum + 0.000_1);
            L_out := L_in;
            L_out := L_out / (1.0 + L_out);

            -- Change the luminance
            Set_Lum (xyY, L_out);

            XYZ := Convert_xyY_Spectrum (xyY);
            RGB := Convert_XYZ_Spectrum (XYZ);
            RGB := Gamma_Correct (RGB, GAMMA_CORRECTION);
            Col := Convert_RGB_Spectrum (RGB);
            Set_Pixel_With_Buffer (X, Y, Get_R (Col), Get_G (Col), Get_B (Col));
         end loop;
      end loop;

      Put_Line ("DONE Computing Reinhard Tone Mapping");
      New_Line;

   end Map_Tones_To_Image_Reinhard;

   procedure Map_Tones_To_Image_Reinhard_Extended is
      Col                  : RGB_PixelColor;
      RGB                  : RGB_Spectrum;
      XYZ                  : XYZ_Spectrum;
      xyY                  : xyY_Spectrum;
      total_Lum            : Large_Float;
      avg_Lum, L_in, L_out : Small_Float;
      L_white              : Small_Float;
   begin
      New_Line;
      Put_Line ("Computing Reinhard Extended Tone Mapping");

      -- Compute total and average luminance
      Put_Line ("Computing average luminance");
      total_Lum := 0.0;
      for Y in 0 .. Main_Tone_Map.YRes - 1 loop
         for X in 0 .. Main_Tone_Map.XRes - 1 loop
            total_Lum := total_Lum + Large_Float (Main_Tone_Map.ToneBuffer (X, Y).Luminance);
         end loop;
      end loop;
      avg_Lum := Small_Float (total_Lum / Large_Float ((Main_Tone_Map.XRes * Main_Tone_Map.YRes)));
      Put_Line ("Average Luminance = " & avg_Lum'Image);

      L_white := Main_Tone_Map.Imax;

      --  Loop through all pixels
      for Y in 0 .. Main_Tone_Map.YRes - 1 loop
         for X in 0 .. Main_Tone_Map.XRes - 1 loop

            -- RGB to XYZ to xyY
            Debug_Spectrum (Main_Tone_Map.ToneBuffer (X, Y).Radiance);
            XYZ := Convert_RGB_Spectrum (Main_Tone_Map.ToneBuffer (X, Y).Radiance);
            Debug_Spectrum (XYZ);
            xyY := Convert_XYZ_Spectrum (XYZ);
            Debug_Spectrum (xyY);

            -- See https://bruop.github.io/tonemapping/
            -- See http://www.brucelindbloom.com/index.html?Math.html
            L_in := Get_Lum (xyY);
            -- Exposure correction or not?
            -- L_out := L_in / (9.6 * avg_Lum + 0.000_1);
            L_out := L_in;
            L_out := L_out * (1.0 + (L_out / (L_white * L_white))) / (1.0 + L_out);
            Set_Lum (xyY, L_out);

            XYZ := Convert_xyY_Spectrum (xyY);
            RGB := Convert_XYZ_Spectrum (XYZ);
            RGB := Gamma_Correct (RGB, GAMMA_CORRECTION);
            Col := Convert_RGB_Spectrum (RGB);
            Set_Pixel_With_Buffer (X, Y, Get_R (Col), Get_G (Col), Get_B (Col));
         end loop;
      end loop;

      Put_Line ("DONE Computing Reinhard Extended Tone Mapping");
      New_Line;

   end Map_Tones_To_Image_Reinhard_Extended;

end Tone_Maps;
