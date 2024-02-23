with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Tracers;             use Tracers;
with Utilities;           use Utilities;
with Ada.Tags;            use Ada.Tags;

package body Cameras is

   ------------------
   -- ADT Camera
   ------------------

   function Construct_Basic_Camera
     (Position        : in Point_3D;
      Look_At         : in Point_3D;
      Up              : in Vector_3D;
      X_Res, Y_Res    : in Positive;
      Width           : in Large_Float;
      Screen_Distance : in Large_Float) return Camera_Ptr is
      Cam_Ptr : Camera_Ptr;
   begin

      Cam_Ptr := new Camera;

      Set_Viewing_Params (Cam_Ptr, Position, Look_At, Up);
      Set_Screen_Params (Cam_Ptr, X_Res, Y_Res, Width, Screen_Distance);
      Set_Filter (Cam_Ptr.all, Construct_Box_Filter);

      return Cam_Ptr;
   end Construct_Basic_Camera;

   procedure Put (Cam : in Camera'Class; Msg : in String := "Camera = ") is
   begin
      Put (Msg);
      New_Line;
      Put (Cam.U, "U = ");
      Put (Cam.V, "V = ");
      Put (Cam.W, "W = ");
      Put (Cam.Position, "Position = ");
      Put (Cam.Look_At, "Look_At = ");
      Put (Cam.Up, "Up = ");
      Put (Cam.LL_Screen_Corner, "LL Corner = ");
      Put (Cam.H_Screen_Vec, "Hor Vec = ");
      Put (Cam.V_Screen_Vec, "Ver Vec = ");
      Put ("Distance = ");
      Put (Cam.Screen_Distance);
      New_Line;
      Put ("X_Res = ");
      Put (Cam.X_Res);
      New_Line;
      Put ("Y_Res = ");
      Put (Cam.Y_Res);
      New_Line;
      Put ("Delta_X = ");
      Put (Cam.Delta_X);
      New_Line;
      Put ("Delta_Y = ");
      Put (Cam.Delta_Y);
      New_Line;
   end Put;

   procedure Set_Viewing_Params (Cam_Ptr : in Camera_Ptr; Position, Look_At : in Point_3D; Up : in Vector_3D) is
   begin

      --  Set the viewing parameters
      Cam_Ptr.Position := Position;
      Cam_Ptr.Look_At  := Look_At;
      Cam_Ptr.Up       := Up;

      --  Compute the orthonormal basis
      Cam_Ptr.W := -Normalize (To_Vector_3D (Look_At) - To_Vector_3D (Position));
      Cam_Ptr.U := Normalize (Up**Cam_Ptr.W);
      Cam_Ptr.V := Cam_Ptr.W**Cam_Ptr.U;

   end Set_Viewing_Params;

   procedure Set_Screen_Params
     (Cam_Ptr      : in Camera_Ptr;
      X_Res, Y_Res : in Positive;
      Width        : in Large_Float;
      Distance     : in Large_Float) is
      Screen_Ratio   : Large_Float;
      U0, U1, V0, V1 : Large_Float;
      Cen            : Vector_3D;
   begin

      --  Set the screen parameters
      Cam_Ptr.X_Res           := X_Res;
      Cam_Ptr.Y_Res           := Y_Res;
      Cam_Ptr.Screen_Distance := Distance;

      --  We assume the horizontal part of the screen window runs from -Width/2 to Width/2. We compute the height based on the
      --  screen ratio, assuming square and symmetric pixels
      U1           := Width * 0.5;
      U0           := -U1;
      Screen_Ratio := Large_Float (X_Res) / Large_Float (Y_Res);
      V0           := U0 / Screen_Ratio;
      V1           := U1 / Screen_Ratio;

      Cam_Ptr.H_Screen_Vec := (U1 - U0) * Cam_Ptr.U;
      Cam_Ptr.V_Screen_Vec := (V1 - V0) * Cam_Ptr.V;

      Cam_Ptr.Delta_X := 1.0 / Large_Float (Cam_Ptr.X_Res);
      Cam_Ptr.Delta_Y := 1.0 / Large_Float (Cam_Ptr.Y_Res);

      --  Lower-Left Screen Corner is center of the screen and then half the width to the left and half the height down
      Cen                      := To_Vector_3D (Cam_Ptr.Position) - Cam_Ptr.Screen_Distance * Cam_Ptr.W;
      Cam_Ptr.LL_Screen_Corner := To_Point_3D (Cen - 0.5 * (U1 - U0) * Cam_Ptr.U - 0.5 * (V1 - V0) * Cam_Ptr.V);

   end Set_Screen_Params;

   function Get_Delta_X (Cam : in Camera) return Large_Float is (Cam.Delta_X);

   function Get_Delta_Y (Cam : in Camera) return Large_Float is (Cam.Delta_Y);

   function Get_X_Res (Cam : in Camera) return Positive is (Cam.X_Res);

   function Get_Y_Res (Cam : in Camera) return Positive is (Cam.Y_Res);

   function Get_Hor_Screen_Vec (Cam : in Camera) return Vector_3D is (Cam.H_Screen_Vec);

   function Get_Ver_Screen_Vec (Cam : in Camera) return Vector_3D is (Cam.V_Screen_Vec);

   function Get_LL_Screen_Corner (Cam : in Camera) return Point_3D is (Cam.LL_Screen_Corner);

   function Get_Position (Cam : in Camera) return Point_3D is (Cam.Position);

   procedure Set_Pixel_Sampler (Cam : in out Camera; Smp_Ptr : in Sampler_Ptr) is
   begin
      Cam.Pixel_Sampler := Smp_Ptr;
   end Set_Pixel_Sampler;

   function Get_Pixel_Sampler (Cam : in out Camera) return Sampler_Ptr is (Cam.Pixel_Sampler);

   function Get_Ray_For_Next_Pixel_Sample (Cam : in Camera; X, Y : in Natural) return Ray is
      R      : Ray;
      Sample : Point_2D;
      A, B   : Large_Float;
      S      : Vector_3D;
   begin
      --  Get the sample in a unit square
      Sample := Cam.Pixel_Sampler.Get_Next_Sample;

      --  Map the pixel coordinates to screen window coordinates
      A := Cam.Delta_X * (Get_X (Sample) + Large_Float (X));
      B := Cam.Delta_Y * (Get_Y (Sample) + Large_Float (Y));

      --  Compute the screen coordinate in World coordinates
      S := To_Vector_3D (Cam.LL_Screen_Corner) + A * Cam.H_Screen_Vec + B * Cam.V_Screen_Vec;

      --  Compute the ray as an offset of the screen, all rays starting parallel to the viewing direction
      R := Construct_Ray (To_Point_3D (S) + Cam.Screen_Distance * Cam.W, -Cam.W);

      return R;
   end Get_Ray_For_Next_Pixel_Sample;

   procedure Set_Pixel_Sample (Cam : in out Camera'Class; Nr : in Integer; Radiance : in RGB_Spectrum; Position : Point_2D) is
   begin
      Cam.Samples (Nr).Radiance := Radiance;
      Cam.Samples (Nr).Position := Position;
   end Set_Pixel_Sample;

   function Sample_Pixel (Cam : in out Camera'Class; X, Y : in Natural) return RGB_Spectrum is
      Avg_Radiance  : RGB_Spectrum := BLACK_RGB_Spec;
      Prim_Ray      : Ray;
      No_Of_Samples : Natural;
   begin
      --  Initialize
      No_Of_Samples := Cam.Pixel_Sampler.Get_No_Of_Samples;

      --  Loop through the number of samples
      for i in 1 .. No_Of_Samples loop
         Prim_Ray := Get_Ray_For_Next_Pixel_Sample (Cam, X, Y);
         Set_Pixel_Sample (Cam, i, Trace_Ray (Prim_Ray), Cam.Pixel_Sampler.Get_Current_Sample);
         Number_Of_Primary_Rays := Number_Of_Primary_Rays + 1;
      end loop;

      -- Filter the pixel samples
      return Cam.Get_Filter.Filter_Samples (Cam.Samples, No_Of_Samples);

   end Sample_Pixel;

   ------------------
   -- ADT Filter
   ------------------
   procedure Put (Flter : in Filter'Class; Msg : in String := "Filter = ") is
   begin
      Put_Line ("BEGIN " & Msg);
      Put ("Class = ");
      Put (External_Tag (Flter'Tag));
      New_Line;
      Put ("Name = ");
      Put (Ada.Strings.Unbounded.To_String (Flter.Name));
      New_Line;
      Put_Line ("END " & Msg);
   end Put;

   function Construct_Box_Filter return Filter_Ptr is
      New_Filter_Ptr : Filter_Ptr;
   begin
      New_Filter_Ptr      := new Box_Filter;
      New_Filter_Ptr.Name := To_Unbounded_String ("Box Filter");
      return New_Filter_Ptr;
   end Construct_Box_Filter;

   function Construct_MultiStage_Filter return Filter_Ptr is
      New_Filter_Ptr : Filter_Ptr;
   begin
      New_Filter_Ptr      := new MultiStage_Filter;
      New_Filter_Ptr.Name := To_Unbounded_String ("MultiStage Filter");
      return New_Filter_Ptr;
   end Construct_MultiStage_Filter;

   function Construct_Poisson_Filter return Filter_Ptr is
      New_Filter_Ptr : Filter_Ptr;
   begin
      New_Filter_Ptr      := new Poisson_Filter;
      New_Filter_Ptr.Name := To_Unbounded_String ("Poisson Filter");
      return New_Filter_Ptr;
   end Construct_Poisson_Filter;

   procedure Set_Filter (Cam : in out Camera; Flt_Ptr : in Filter_Ptr) is
   begin
      Cam.Flt_Ptr := Flt_Ptr;
   end Set_Filter;

   function Get_Filter (Cam : in Camera) return Filter_Ptr is (Cam.Flt_Ptr);

   function Filter_Samples (Flter : in Filter; Samples : Pixel_Samples; NumSamples : Positive) return RGB_Spectrum is
   begin
      Put_Line ("*** ERROR: Illegal call to Filter_Samples class-wide function");
      return BLACK_RGB_Spec;
   end Filter_Samples;

   function Filter_Samples (Flter : in Box_Filter; Samples : Pixel_Samples; NumSamples : Positive) return RGB_Spectrum is
      Avg_Rad : RGB_Spectrum := BLACK_RGB_Spec;
   begin
      --  First add all the samples evenly weighted
      for I in 1 .. NumSamples loop
         Avg_Rad := Avg_Rad + Samples (I).Radiance;
      end loop;
      --  Now divide by the number of samples
      return Avg_Rad * Small_Float (1.0 / Small_Float (NumSamples));
   end Filter_Samples;

   function Filter_Samples (Flter : in MultiStage_Filter; Samples : Pixel_Samples; NumSamples : Positive) return RGB_Spectrum is
      Avg_Rad    : RGB_Spectrum := BLACK_RGB_Spec;
      Grid_Rad   : array (1 .. 16) of RGB_Spectrum;
      Grid_Count : array (1 .. 16) of Integer;
      Xs, Ys     : Large_Float;
      Xg, Yg, Gc : Integer      := 0;
   begin

      --  Initialize
      for i in 1 .. 16 loop
         Grid_Rad (i)   := BLACK_RGB_Spec;
         Grid_Count (i) := 0;
      end loop;

      --  Loop through each sample and allocate it to a grid
      for i in 1 .. NumSamples loop
         Xs := Get_X (Samples (i).Position);
         Ys := Get_Y (Samples (i).Position);

         --  Determine the grid number
         if Xs < 0.25 then
            Xg := 1;
         else
            if Xs < 0.5 then
               Xg := 2;
            else
               if Xs < 0.75 then
                  Xg := 3;
               else
                  Xg := 4;
               end if;
            end if;
         end if;
         if Ys < 0.25 then
            Yg := 0;
         else
            if Ys < 0.5 then
               Yg := 4;
            else
               if Ys < 0.75 then
                  Yg := 8;
               else
                  Yg := 12;
               end if;
            end if;
         end if;

         --  Update the grid
         Grid_Rad (Yg + Xg)   := Grid_Rad (Yg + Xg) + Samples (i).Radiance;
         Grid_Count (Yg + Xg) := Grid_Count ((Yg + Xg)) + 1;
      end loop;

      --  Now compute the average for each grid that has one or more samples
      for i in 1 .. 16 loop
         if Grid_Count (i) > 0 then
            Grid_Rad (i) := (1.0 / Small_Float (Grid_Count (i))) * Grid_Rad (i);
         end if;
      end loop;

      --  Now compute the average of all grids containing a sample
      for i in 1 .. 16 loop
         if Grid_Count (i) > 0 then
            Avg_Rad := Avg_Rad + Grid_Rad (i);
            Gc      := Gc + 1;
         end if;
      end loop;
      Avg_Rad := (1.0 / Small_Float (Gc)) * Avg_Rad;

      return Avg_Rad;
   end Filter_Samples;

   function Filter_Samples (Flter : in Poisson_Filter; Samples : Pixel_Samples; NumSamples : Positive) return RGB_Spectrum is
      Avg_Rad : RGB_Spectrum := PURPLE_RGB_Spec;
   begin
      return Avg_Rad;
   end Filter_Samples;
end Cameras;
