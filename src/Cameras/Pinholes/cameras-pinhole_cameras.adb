with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Tracers;             use Tracers;

package body Cameras.Pinhole_Cameras is

   function Construct_Pinhole_Camera
     (Position        : in Point_3D;
      Look_At         : in Point_3D;
      Up              : in Vector_3D;
      X_Res, Y_Res    : in Positive;
      Width           : in Large_Float;
      Screen_Distance : in Large_Float) return Camera_Ptr is
      Cam_Ptr : Camera_Ptr;
   begin

      Cam_Ptr := new Pinhole_Camera;

      Set_Viewing_Params (Cam_Ptr, Position, Look_At, Up);
      Set_Screen_Params (Cam_Ptr, X_Res, Y_Res, Width, Screen_Distance);
      Set_Filter (Cam_Ptr.all, Construct_Box_Filter);

      return Cam_Ptr;
   end Construct_Pinhole_Camera;

   function Get_Ray_For_Next_Pixel_Sample (Cam : in Pinhole_Camera; X, Y : in Natural) return Ray is
      R        : Ray;
      Sample   : Point_2D;
      A, B     : Large_Float;
      S        : Vector_3D;
      Position : Point_3D;
   begin
      --  Get the sample in a unit square
      Sample := Cam.Pixel_Sampler.Get_Next_Sample;

      --  Map the pixel coordinates to screen window coordinates
      A := Cam.Delta_X * (Get_X (Sample) + Large_Float (X));
      B := Cam.Delta_Y * (Get_Y (Sample) + Large_Float (Y));

      --  Compute the screen coordinate in World coordinates
      S := To_Vector_3D (Cam.Get_LL_Screen_Corner) + A * Cam.Get_Hor_Screen_Vec + B * Cam.Get_Ver_Screen_Vec;
      --  Compute the ray as an offset of the screen
      Position := Cam.Get_Position;
      R        := Construct_Ray (Position, S - To_Vector_3D (Position));

      return R;
   end Get_Ray_For_Next_Pixel_Sample;

end Cameras.Pinhole_Cameras;
