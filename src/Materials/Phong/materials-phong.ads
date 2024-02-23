--<summary>The Materials.Phong package contains all Abstract Data Types for generic material handling</summary>
--<description>The Materials package contains all Abstract Data Types for generic material handling.
--</description>
--<group>Materials</group>
package Materials.Phong is

   ------------------
   --  ADT Phong
   ------------------
   type Phong is new Material with private;

   type Phong_Ptr is access all Phong;
   --<summary>The access type to all Phong materials</summary>

   function Construct_Phong
     (Name                           : in String;
      Amb_Spec, Diff_Spec, Spec_Spec : in RGB_Spectrum;
      Ka, Kd, Ks                     : in Small_Float := 1.0;
      Alpha                          : in Integer     := 1) return Material_Ptr;
   --<summary>Constructs a Phong material</summary>
   --<description>Constructs a Phong material</description>
   --<parameter name="Name">The name of the material</parameter>
   --<parameter name="Amb_Spec">The ambient reflection spectrum of the material</parameter>
   --<parameter name="Diff_Spec">The diffuse reflection spectrum of the material</parameter>
   --<parameter name="Spec_Spec">The specular reflection spectrum of the material</parameter>
   --<parameter name="Ka">The ambient reflection coefficient</parameter>
   --<parameter name="Kd">The diffuse reflection coefficient</parameter>
   --<parameter name="Ks">The specular reflection coefficient</parameter>
   --<parameter name="Alpha">The shinyness power coefficient of the material</parameter>
   --<exception>None at this moment</exception>

   function BDRF (Mat : in Phong; In_Ray : in Ray; Hp : in HitPoint; Weight : in Small_Float) return RGB_Spectrum;
   --<summary>Computes the BDRF value for a material at a hitpoint for an incoming ray</summary>
   --<description>Computes the BDRF value for a material at a hitpoint for an incoming ray</description>
   --<parameter name="Mat">A material of the Material Class Wide Type</parameter>
   --<parameter name="In_Ray">The incoming ray for this hitpoint</parameter>
   --<parameter name="Hp">The hitpoint</parameter>
   --<parameter name="Weight">The weight for this ray, used to trim the recursion</parameter>
   --<exception>None at this moment</exception>

private

   type Phong is new Material with record
      Ambient_Reflection_Spectrum  : RGB_Spectrum;
      Diffuse_Reflection_Spectrum  : RGB_Spectrum;
      Specular_Reflection_Spectrum : RGB_Spectrum;
      K_amb                        : Small_Float := 1.0;
      K_diff                       : Small_Float := 1.0;
      K_spec                       : Small_Float := 1.0;
      Alpha                        : Integer     := 1;
   end record;

end Materials.Phong;
