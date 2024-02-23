with Text_IO;                 use Text_IO;
with Ada.Integer_Text_IO;     use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with M_GraphiX;               use M_GraphiX;
with Core_Types;              use Core_Types;
with Linear_Math;             use Linear_Math;
with Objects;                 use Objects;
with Objects.Unit_Cubes;      use Objects.Unit_Cubes;
with Objects.Unit_Spheres;    use Objects.Unit_Spheres;
with Objects.Unit_Cylinders;  use Objects.Unit_Cylinders;
with Objects.Unit_Cones;      use Objects.Unit_Cones;
with Materials;               use Materials;
with Materials.Lambertian;    use Materials.Lambertian;
with Lights;                  use Lights;
with Lights.Directionals;     use Lights.Directionals;
with Cameras;                 use Cameras;
with Cameras.Pinhole_Cameras; use Cameras.Pinhole_Cameras;
with Spectra;                 use Spectra;
with HitPoints;               use HitPoints;
with Small_Float_Functions;   use Small_Float_Functions;
with Normal_Float_Functions;  use Normal_Float_Functions;
with Large_Float_Functions;   use Large_Float_Functions;

package body Core_Tests is

   procedure RGB_Test is
      Sp1, Sp2            : RGB_Spectrum;
      Color               : RGB_PixelColor;
      R, G, B, dR, dG, dB : Small_Float;
   begin
      Open_Image_Window (1_024, 1_024, True, To_Unbounded_String ("Spectrum Test"));
      dR := 1.0 / 255.0;
      dG := 1.0 / 255.0;
      dB := 16.0 / 255.0;
      R  := 0.0;
      G  := 0.0;
      B  := 0.0;
      for y in 0 .. 1_023 loop
         for x in 0 .. 1_023 loop
            Sp1   := Construct_RGB_Spectrum (R, G, B);
            Color := Convert_RGB_Spectrum (Sp1);
            Set_Pixel (x, y, Get_R (Color), Get_G (Color), Get_B (Color));
            R := R + dR;
            if R > 1.0 then
               R := 0.0;
               G := G + dG;
            end if;
            if G > 1.0 then
               G := 0.0;
               B := B + dB;
            end if;
            if B > 1.0 then
               B := 0.0;
            end if;
         end loop;
      end loop;
      Put (GfxLib_WaitForEvent);
      GfxLib_CloseWindow;
   end RGB_Test;

   procedure Sine_Wave_Pattern is
      col     : Integer;
      u, v, w : Large_Float;
   begin
      Open_Image_Window (600, 600, True, To_Unbounded_String ("Sine Wave Pattern"));
      for y in 0 .. 599 loop
         for x in 0 .. 599 loop
            v := 10.0 * (Large_Float (y)) / 600.0;
            u := 10.0 * (Large_Float (x)) / 600.0;
            w := Sin (u * v * u / (0.000_01 + u - v));
            --  w := Sin (2.0 * (u / v) * (u / v));
            col := Integer (255.0 * w * w);
            Set_Pixel (x, 599 - y, col, col, col);
         end loop;
      end loop;
      Put (GfxLib_WaitForEvent);
      Save_Image (To_Unbounded_String ("/home/mlaban/sine_wave.ppm"));
      GfxLib_CloseWindow;
   end Sine_Wave_Pattern;

   procedure Core_Type_Facts is
      xs : Small_Float;
      xn : Normal_Float;
      xl : Large_Float;
   begin
      Put_Line ("Small_Integer Facts");
      Put ("Size: ");
      Put (Integer (Small_Integer'Size));
      New_Line;
      Put ("First: ");
      Put (Small_Integer'First);
      New_Line;
      Put ("Last: ");
      Put (Small_Integer'Last);
      New_Line;
      Put_Line ("---------------");

      Put_Line ("Normal_Integer Facts");
      Put ("Size: ");
      Put (Integer (Normal_Integer'Size));
      New_Line;
      Put ("First: ");
      Put (Normal_Integer'First);
      New_Line;
      Put ("Last: ");
      Put (Normal_Integer'Last);
      New_Line;
      Put_Line ("--------------");

      Put_Line ("Large_Integer Facts");
      Put ("Size: ");
      Put (Integer (Large_Integer'Size));
      New_Line;
      Put ("First: ");
      Put (Large_Integer'First);
      New_Line;
      Put ("Last: ");
      Put (Large_Integer'Last);
      New_Line;
      Put_Line ("--------------");

      Put_Line ("Huge_Integer Facts");
      Put ("Size: ");
      Put (Integer (Huge_Integer'Size));
      New_Line;
      Put ("First: ");
      Put (Huge_Integer'First);
      New_Line;
      Put ("Last: ");
      Put (Huge_Integer'Last);
      New_Line;
      Put_Line ("--------------");

      Put_Line ("Small_Float Facts");
      Put ("Size: ");
      Put (Integer (Small_Float'Size));
      New_Line;
      Put ("First: ");
      Put (Small_Float'First);
      New_Line;
      Put ("Last: ");
      Put (Small_Float'Last);
      xs := 0.0;
      New_Line;
      Put ("1.0 / 0.0 : ");
      Put (1.0 / xs);
      New_Line;
      Put_Line ("---------------");

      Put_Line ("Normal_Float Facts");
      Put ("Size: ");
      Put (Integer (Normal_Float'Size));
      New_Line;
      Put ("First: ");
      Put (Normal_Float'First);
      New_Line;
      Put ("Last: ");
      Put (Normal_Float'Last);
      xn := 0.0;
      New_Line;
      Put ("1.0 / 0.0 : ");
      Put (1.0 / xn);
      New_Line;
      Put_Line ("---------------");

      Put_Line ("Large_Float Facts");
      Put ("Size: ");
      Put (Integer (Large_Float'Size));
      New_Line;
      Put ("First: ");
      Put (Large_Float'First);
      New_Line;
      Put ("Last: ");
      Put (Large_Float'Last);
      xl := 0.0;
      New_Line;
      Put ("1.0 / 0.0 : ");
      Put (1.0 / xl);
      New_Line;
      Put_Line ("---------------");

   end Core_Type_Facts;

   procedure Math_Performance_Test is
      v1, v2 : Small_Float;
   begin
      Put (Integer (Small_Float'Size));
      New_Line;
      Put (Small_Float'First);
      New_Line;
      for x in 0 .. 30_000_000 loop
         v1 := Small_Float (x) / 123.456;
         v2 := (v1 + Sin (v1) * Cos (v1) * Sqrt (v1)) / 3.141_527;
      end loop;
      Put (v2);
   end Math_Performance_Test;

   procedure Math_Multiply_Perf_Test is
      v1, v2 : Large_Float;
   begin
      v1 := 1.0;
      v2 := 1.0 / 0.999_999;
      for x in 0 .. 1_000_000_000 loop
         v1 := v1 / v2;
      end loop;
      Put (v1);
      New_Line;
   end Math_Multiply_Perf_Test;

   procedure Linear_Math_Type_Test is
      P2D : Point_2D;
      P3D : Point_3D;
      V3D : Vector_3D;
      N3D : Normal_3D;
      M3D : Matrix_3D;
   begin
      P2D := Construct_Point (Ray_PI, Ray_PI_div4);
      Put (P2D, "my Point_2D = ");
      P3D := Construct_Point (Ray_PI, Ray_PI_div4, -Ray_PI_div2);
      Put (P3D, "my Point_3D = ");
      V3D := Construct_Vector (Ray_PI, Ray_PI_div4, -Ray_PI_div2);
      Put (V3D, "my Vector_3D = ");
      N3D := Construct_Normal (1.0, 1.1, 1.2);
      Put (N3D, "my Vector_3D = ");
      Put (M3D, "my Matrix_3D = ");
   end Linear_Math_Type_Test;

   procedure Linear_Math_Ops_Test is
      P1, P2     : Point_3D;
      V1, V2     : Vector_3D;
      N1, N2     : Normal_3D;
      M1, M2, M3 : Matrix_3D;
      R          : Ray;
   begin
      P1 := Construct_Point (1.0, 0.0, 0.0);
      P2 := Construct_Point (-1.0, 1.0, -1.0);
      V1 := Construct_Vector (0.0, 1.0, 0.0);
      V2 := Construct_Vector (-1.0, -2.0, -3.0);
      N1 := Construct_Normal (1.0, 1.0, 0.0);
      Put (N1);
      N2 := Construct_Normal (-1.0, -2.0, -3.0);

      R := Construct_Ray (P2, V2);
      --  M1 := Construct_Rotate_Z (Ray_PI_div4);
      M2 := Construct_Scale (2.0, 1.0, 1.0);
      M3 := M2 * M1;
      Put (M3, "M3 = ");
      --  Put (Inverse (M3), "Inv M3 = ");
      Put (Inverse (M3) * N1, "N1 Transformed = ");

--        Put (X_AXIS_3D**Y_AXIS_3D, "X x Y = ");
--        Put (Y_AXIS_3D**X_AXIS_3D, "Y x X = ");
--        Put (Y_AXIS_3D**Z_AXIS_3D, "Y x Z = ");
--        Put (Z_AXIS_3D**Y_AXIS_3D, "Z x Y = ");
--        Put (Z_AXIS_3D**X_AXIS_3D, "Z x X = ");
--        Put (X_AXIS_3D**Z_AXIS_3D, "X x Z = ");

   end Linear_Math_Ops_Test;

   procedure Objects_Test is
      Obj      : Object_Ptr;
      Obj_List : Object_List;
      Hp       : HitPoint;
      M        : Matrix_3D;
   begin
      Obj := new Object;
      M   := Construct_Scale (2.0, 3.0, 4.0);
      Obj.Set_Object_Name (To_Unbounded_String ("Generic Object Name"));
      Obj.Transform (M);
      Put (Hp);
   end Objects_Test;

end Core_Tests;
