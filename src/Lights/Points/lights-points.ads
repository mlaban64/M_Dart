--<summary>The Point_Lights package contains all Abstract Data Types for handling of point lights</summary>
--<description>The Points_Lights package contains all Abstract Data Types for handling of point lights--</description>
--<group>Lights</group>

package Lights.Points is

   -------------------
   --  ADT Point_Light
   -------------------

   type Point_Light is new Light with private;
   --<summary>ADT of the point lights</summary>

   type Point_Light_Ptr is access all Point_Light;
   --<summary>The access type to Point_Light objects</summary>

   function Construct_Point_Light
     (Name           : in String;
      Exitant_Energy : in Small_Float;
      Exitant_Color  : in RGB_Spectrum;
      Position       : in Point_3D;
      Shadow         : in Boolean) return Light_Ptr;
   --<summary>Creates a point light</summary>
   --<description>Function that creates a point light</description>
   --<parameter name="Name">The Name of the light</parameter>
   --<parameter name="Exitant_Energy">The overall energy of the light</parameter>
   --<parameter name="Exitant_Color">The Name of the light</parameter>
   --<parameter name="Position">The point where the light is located</parameter>
   --<parameter name="Shadow">Does this light cast a shadow?</parameter>
   --<exception>None at this moment</exception>

   procedure Put (Lt : in Point_Light; Msg : in String := "Point Light");
   --<summary>Prints a Point_Light's statistics</summary>
   --<description>Prints a Point_Light's statistics</description>
   --<parameter name="Lt">A Point_Light</parameter>
   --<exception>None at this moment</exception>

   function Get_Normal_For_Next_Light_Sample (Lt : in Point_Light; Hp : in HitPoint) return Normal_3D;
   --<summary>returns a Normal pointing to the direction where the light is coming from for the next light sample</summary>
   --<description>returns a Normal pointing to the direction where the light is coming from for the next light sample.
   --Hence it is opposite to the direction the light is travelling to</description>
   --<parameter name="Lt">A point light</parameter>
   --<parameter name="Hp">The hitpoint go get the normal for</parameter>
   --<exception>None at this moment</exception>

   function Incident_Radiance (Lt : in Point_Light; Hp : in HitPoint; Dir : Normal_3D) return RGB_Spectrum;
   --<summary>Function to compute the incoming light for a hitpoint for a point light source</summary>
   --<description>Function to compute the incoming light for a hitpoint for a directional light source</description>
   --<parameter name="Lt">The directional light</parameter>
   --<parameter name="Hp">Pointer to the hitpoint for which we need to determine the incident radiance coming from this lightsource</parameter>
   --<parameter name="Dir">Normal poiArrow_Ptr4nting to the direction where the light is coming from</parameter>
   --<exception>None at this moment</exception>on>

private

   type Point_Light is new Light with record
      Exitant_Energy : Small_Float;
      --  The energy emitted per direction
      Exitant_Color : RGB_Spectrum;
      --  The color of the emitted light
      Position : Point_3D;
      --  The direction the light is travelling to
   end record;

end Lights.Points;
