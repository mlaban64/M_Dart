with Linear_Math;           use Linear_Math;
with HitPoints;             use HitPoints;
with Spectra;               use Spectra;
with Core_Types;            use Core_Types;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--<summary>The Materials package contains all Abstract Data Types to handle materials</summary>
--<description>The Materials package contains all Abstract Data Types to handle materials.
--The actual material definitions and methods for a particular material type will be handled in separate child packages</description>
--<group>Materials</group>
package Materials is

   ----------------
   --  ADT Material
   ----------------

   type Material is tagged private;
   --<summary>ADT of the root type of all materials. It is tagged so it can be used as a Class for dispatching</summary>

   type Material_Ptr is access all Material'Class;
   --<summary>The access type to all Material'Class objects</summary>

   --------------------------
   --  ADT Material Functions
   --------------------------
   procedure Put (Mat : in Material'Class; Msg : in String := "Material = ");
   --<summary>Prints a Material's statistics</summary>
   --<description>Prints a Material's statistics</description>
   --<parameter name="Mat">A material of the Material Class Wide Type</parameter>
   --<exception>None at this moment</exception>

   function Max_Weight_Spectrum (Weight : in Small_Float; Factor : in Small_Float; Spectrum : RGB_Spectrum) return Small_Float;
   --<summary>Computes the maximum weight a possible traced ray can have</summary>
   --<description>Computes the maximum weight a possible traced ray can have</description>
   --<parameter name="Weight">The weight for this ray</parameter>
   --<parameter name="Factor">The k-factor for this ray, usually k_refl or k_tran</parameter>
   --<parameter name="Spectrum">The spectrum applying to the ray</parameter>
   --<exception>None at this moment</exception>

   function BDRF (Mat : in Material; In_Ray : in Ray; Hp : in HitPoint; Weight : in Small_Float) return RGB_Spectrum;
   --<summary>Computes the BDRF value for a material at a hitpoint for an incoming ray</summary>
   --<description>Computes the BDRF value for a material at a hitpoint for an incoming ray</description>
   --<parameter name="Mat">A material of the Material Class Wide Type</parameter>
   --<parameter name="In_Ray">The incoming ray for this hitpoint</parameter>
   --<parameter name="Hp">The hitpoint</parameter>
   --<parameter name="Weight">The weight for this ray, used to trim the recursion</parameter>
   --<exception>None at this moment</exception>

   function BDTF (Mat : in Material; In_Ray : in Ray; Hp : in HitPoint; Weight : in Small_Float) return RGB_Spectrum;
   --<summary>Computes the BDTF value for a material at a hitpoint for an incoming ray</summary>
   --<description>Computes the BDTF value for a material at a hitpoint for an incoming ray</description>
   --<parameter name="Mat">A material of the Material Class Wide Type</parameter>
   --<parameter name="In_Ray">The incoming ray for this hitpoint</parameter>
   --<parameter name="Hp">The hitpoint</parameter>
   --<parameter name="Weight">The weight for this ray, used to trim the recursion</parameter>
   --<exception>None at this moment</exception>

private

   type Material is tagged record
      Name : Unbounded_String := To_Unbounded_String ("NOT DEFINED");
      --  The Name of the Material
   end record;

end Materials;
