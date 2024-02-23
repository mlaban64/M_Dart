--<summary>The Directional_Lights package contains all Abstract Data Types for handling of directional lights</summary>
--<description>The Directional_Lights package contains all Abstract Data Types for handling of directional lights--</description>
--<group>Lights</group>

package Lights.Directionals is

   -------------------------
   --  ADT Directional_Light
   -------------------------

   type Directional_Light is new Light with private;
   --<summary>ADT of the directional lights</summary>

   type Directional_Light_Ptr is access all Directional_Light;
   --<summary>The access type to Directional_Light objects</summary>

   function Construct_Directional_Light
     (Name           : in String;
      Exitant_Energy : in Small_Float;
      Exitant_Color  : in RGB_Spectrum;
      Direction      : in Vector_3D;
      Shadow         : in Boolean) return Light_Ptr;
   --<summary>Creates a Directional Light</summary>
   --<description>Function that creates a directional light</description>
   --<parameter name="Name">The Name of the light</parameter>
   --<parameter name="Exitant_Energy">The overall energy of the light</parameter>
   --<parameter name="Exitant_Color">The Name of the light</parameter>
   --<parameter name="Direction">The directional vector the light is travelling to</parameter>
   --<parameter name="Shadow">Does this light cast a shadow?</parameter>
   --<exception>None at this moment</exception>

   procedure Put (Lt : in Directional_Light; Msg : in String := "Directional Light");
   --<summary>Prints a Directional_Light's statistics</summary>
   --<description>Prints a Directional_Light's statistics</description>
   --<parameter name="Lt">A Directional_Light</parameter>
   --<exception>None at this moment</exception>

   function Get_Normal_For_Next_Light_Sample (Lt : in Directional_Light; Hp : in HitPoint) return Normal_3D;
   --<summary>returns a Normal pointing to the direction where the light is coming from for the next light sample</summary>
   --<description>returns a Normal pointing to the direction where the light is coming from for the next light sample.
   --Hence it is opposite to the direction the light is travelling to</description>
   --<parameter name="Lt">A directional light</parameter>
   --<parameter name="Hp">The hitpoint go get the normal for</parameter>
   --<exception>None at this moment</exception>

   function Incident_Radiance (Lt : in Directional_Light; Hp : in HitPoint; Dir : Normal_3D) return RGB_Spectrum;
   --<summary>Function to compute the incoming light for a hitpoint for a directional light source</summary>
   --<description>Function to compute the incoming light for a hitpoint for a directional light source</description>
   --<parameter name="Lt">The directional light</parameter>
   --<parameter name="Hp">Pointer to the hitpoint for which we need to determine the incident radiance coming from this lightsource</parameter>
   --<parameter name="Dir">Normal pointing to the direction where the light is coming from</parameter>
   --<exception>None at this moment</exception>on>

private

   type Directional_Light is new Light with record
      Exitant_Energy : Small_Float;
      --  The energy emitted per direction
      Exitant_Color : RGB_Spectrum;
      --  The color of the emitted light
      Direction : Vector_3D;
      --  The direction the light is travelling to
      Dir_Normal : Normal_3D;
      --  The normalized direction the light is travelling to, which can be used for dot products with surface normals
   end record;

end Lights.Directionals;
