with M_GraphiX;           use M_GraphiX;
with Utilities;           use Utilities;
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Tone_Maps is

   procedure Create_Tone_Map (XRes, YRes : in Integer) is
   begin
      Main_Tone_Map      := new Tone_Map (XRes - 1, YRes - 1);
      Main_Tone_Map.XRes := XRes;
      Main_Tone_Map.YRes := YRes;
   end Create_Tone_Map;

   function Refactor_Radiance (Radiance : in RGB_Spectrum) return RGB_Spectrum is
      new_Rad : RGB_Spectrum;
   begin
      new_Rad :=
        Construct_RGB_Spectrum (Get_R (Radiance) * RED_FACTOR, Get_G (Radiance) * GREEN_FACTOR, Get_B (Radiance) * BLUE_FACTOR);
      return new_Rad;
   end Refactor_Radiance;

   procedure Set_Pixel (X, Y : in Integer; Radiance : in RGB_Spectrum) is
      Lum, R, G, B : Small_Float;
   begin
      --  Initialize
      R   := Get_R (Radiance);
      G   := Get_G (Radiance);
      B   := Get_B (Radiance);
      Lum := RED_FACTOR * R + GREEN_FACTOR * G + BLUE_FACTOR * B;

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
      Col    : RGB_PixelColor;
      Factor : Small_Float;
   begin
      New_Line;
      Put_Line ("Computing Linear Tone Mapping");

      --  Get the max R, G or B value
      Factor := Main_Tone_Map.Rmax;
      if Main_Tone_Map.Gmax > Factor then
         Factor := Main_Tone_Map.Gmax;
      end if;
      if Main_Tone_Map.Bmax > Factor then
         Factor := Main_Tone_Map.Bmax;
      end if;

      Factor := Main_Tone_Map.Imax;

      Put_Line ("Tone Mapping Factor = " & Factor'Image);

      if Factor > 0.0 then
         Factor := 1.0 / Factor;

         --  Loop through all pixels
         for Y in 0 .. Main_Tone_Map.YRes - 1 loop
            for X in 0 .. Main_Tone_Map.XRes - 1 loop
               --  Now set the various pixels in the screen and image map
               Col := Convert_RGB_Spectrum (Factor * Main_Tone_Map.ToneBuffer (X, Y).Radiance);
               Set_Pixel_With_Buffer (X, Y, Get_R (Col), Get_G (Col), Get_B (Col));
            end loop;
         end loop;
      else
         Debug_Message ("*** Warning: Factor = 0.0, so image is black");
      end if;

      Put_Line ("DONE Computing Linear Tone Mapping");
      New_Line;

   end Map_Tones_To_Image_Linear;

   procedure Map_Tones_To_Image_Reinhard is
      Col         : RGB_PixelColor;
      Factor      : Small_Float;
      L_in, L_out : Small_Float;
   begin
      New_Line;
      Put_Line ("Computing Reinhard Tone Mapping");

      --  Loop through all pixels
      for Y in 0 .. Main_Tone_Map.YRes - 1 loop
         for X in 0 .. Main_Tone_Map.XRes - 1 loop
            L_in := Main_Tone_Map.ToneBuffer (X, Y).Luminance;

            if L_in > 0.0 then
               L_out  := L_in / (L_in + 1.0);
               Factor := L_out / L_in;
            else
               Factor := 0.0;
            end if;
            --  Now set the various pixels in the screen and image map
            Col := Convert_RGB_Spectrum (Factor * Main_Tone_Map.ToneBuffer (X, Y).Radiance);
            Set_Pixel_With_Buffer (X, Y, Get_R (Col), Get_G (Col), Get_B (Col));
         end loop;
      end loop;

      Put_Line ("DONE Computing Reinhard Tone Mapping");
      New_Line;

   end Map_Tones_To_Image_Reinhard;

   procedure Map_Tones_To_Image_Reinhard_Extended is
      Col                : RGB_PixelColor;
      Factor             : Small_Float;
      L_in, L_out, L_max : Small_Float;
   begin
      New_Line;
      Put_Line ("Computing Reinhard Extended Tone Mapping");

      L_max := Main_Tone_Map.Imax;
      Put_Line ("Max Luminance Factor = " & L_max'Image);

      --  Loop through all pixels
      for Y in 0 .. Main_Tone_Map.YRes - 1 loop
         for X in 0 .. Main_Tone_Map.XRes - 1 loop
            L_in   := Main_Tone_Map.ToneBuffer (X, Y).Luminance;
            L_out  := (L_in * (1.0 + (L_in / (L_max * L_max)))) / (1.0 + L_in);
            Factor := L_out / L_in;
            --  Now set the various pixels in the screen and image map
            Col := Convert_RGB_Spectrum (Factor * Main_Tone_Map.ToneBuffer (X, Y).Radiance);
            Set_Pixel_With_Buffer (X, Y, Get_R (Col), Get_G (Col), Get_B (Col));
         end loop;
      end loop;

      Put_Line ("DONE Computing Reinhard Extended Tone Mapping");
      New_Line;

   end Map_Tones_To_Image_Reinhard_Extended;

end Tone_Maps;
