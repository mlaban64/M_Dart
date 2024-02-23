--<summary>The Pinhole_Cameras package contains the Abstract Data Types for simulation of a pinhole camera</summary>
--<description>The Pinhole_Cameras package contains the Abstract Data Types for simulation of a pinhole camera.</description>
--<group>Cameras</group>
package Cameras.Pinhole_Cameras is

   ------------------
   -- ADT Pinhole_Camera
   ------------------

   type Pinhole_Camera is private;
   --<summary>ADT of the a pinhole camera</summary>

   function Construct_Pinhole_Camera
     (Position        : in Point_3D;
      Look_At         : in Point_3D;
      Up              : in Vector_3D;
      X_Res, Y_Res    : in Positive;
      Width           : in Large_Float;
      Screen_Distance : in Large_Float) return Camera_Ptr;
   --<summary>Returns a Pinhole Camera object</summary>
   --<description>function to setup a pinhole camera</description>
   --<parameter name="Position">The position of the camera</parameter>
   --<parameter name="Look_At">The point where the camera looks at</parameter>
   --<parameter name="Up">The up vector, defining the roll position of the camera</parameter>
   --<parameter name="X_Res">The horizontal screen resolution</parameter>
   --<parameter name="Y_Res">The vertical screen resolution</parameter>
   --<parameter name="Width">The width of the screen in World length</parameter>
   --<parameter name="Screen_Distance">The distance of the view plane from the camera position</parameter>
   --<exception>None at this moment</exception>

   function Get_Ray_For_Next_Pixel_Sample (Cam : in Pinhole_Camera; X, Y : in Natural) return Ray;
   --<summary>Computes a single ray through the center of a pixel</summary>
   --<description>Computes a single ray through the center of a pixel</description>
   --<parameter name="Cam">Pointer to the camera that takes the picture</parameter>
   --<parameter name="X">The X-coordinate of the pixel</parameter>
   --<parameter name="Y">The Y-coordinate of the pixel</parameter>
   --<exception>None at this moment</exception>

private

   type Pinhole_Camera is new Camera with null record;

end Cameras.Pinhole_Cameras;
