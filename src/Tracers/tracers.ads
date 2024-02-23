with Core_Types;  use Core_Types;
with Linear_Math; use Linear_Math;
with Spectra;     use Spectra;
with Materials;   use Materials;

--<summary>The Tracers package contains all the ray tracer routines</summary>
--<description>The Tracers package contains all the ray tracer routines</description>
--<group>Rendering</group>
package Tracers is

   function Trace_Ray (R : in Ray; Weight : in Small_Float := 1.0) return RGB_Spectrum;
   --<summary>Traces a ray through The_World medium and returns the radiance spectrum</summary>
   --<description>Traces a ray through the scene and returns the radiance spectrum</description>
   --<parameter name="R">The ray to cast</parameter>
   --<parameter name="Weight">The weight for this ray, used to trim the recursion</parameter>
   --<exception>None at this moment</exception>

   function Trace_Transmitted_Ray (R : in Ray; Weight : in Small_Float := 1.0) return RGB_Spectrum;
   --<summary>Traces a ray through a transparent medium and returns the radiance spectrum</summary>
   --<description>Traces a ray through the scene and returns the radiance spectrum</description>
   --<parameter name="R">The ray to cast</parameter>
   --<parameter name="Weight">The weight for this ray, used to trim the recursion</parameter>
   --<exception>None at this moment</exception>

   function Trace_Shadow_Ray (R : in Ray) return Large_Float;
   --<summary>Traces a shadow ray through The_World/summary>
   --<description>Traces a shadow ray through The_World and returns a Lambda > 0.0 if something is hit</description>
   --<parameter name="R">The ray to cast</parameter>
   --<exception>None at this moment</exception>

   procedure Trace_Init_Ray (Init_Ray : in Ray);
   --<summary>Traces an initialization ray through The_World/summary>
   --<description>Traces an initialization ray through The_World, so the CSG Tree is set correctly</description>
   --<parameter name="Init_Ray">The ray to cast</parameter>
   --<exception>None at this moment</exception

private

   --  No private part

end Tracers;
