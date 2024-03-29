--<summary>The Materials.Transparent package contains all Abstract Data Types for handling of transparent materials</summary>
--<description>The Materials.Transparent package contains all Abstract Data Types for handling of transparent materials.</description>
--<group>Materials</group>
package Materials.Transparent is

   ------------------
   --  ADT Transparent
   ------------------
   type Transparent is new Material with private;

   type Transparent_Ptr is access all Transparent;
   --<summary>The access type to all Transparent materials</summary>

   function Construct_Transparent
     (Name                                                 : in String;
      Amb_Spec, Diff_Spec, Spec_Spec, Refl_Spec, Tran_Spec : in RGB_Spectrum;
      Ka, Kd, Ks, Kr, Kt                                   : in Small_Float := 1.0;
      Alpha                                                : in Integer     := 1;
      IoR                                                  : in Small_Float := 1.0) return Material_Ptr;
   --<description>Constructs a Transparent material</description>
   --<parameter name="Name">The name of the material</parameter>
   --<parameter name="Amb_Spec">The ambient reflection spectrum of the material</parameter>
   --<parameter name="Diff_Spec">The diffuse reflection spectrum of the material</parameter>
   --<parameter name="Spec_Spec">The specular reflection spectrum of the material</parameter>
   --<parameter name="Refl_Spec">The reflected reflection spectrum of the material</parameter>
   --<parameter name="Refl_Spec">The reflected reflection spectrum of the material</parameter>
   --<parameter name="Ka">The ambient reflection coefficient</parameter>
   --<parameter name="Kd">The diffuse reflection coefficient</parameter>
   --<parameter name="Ks">The specular reflection coefficient</parameter>
   --<parameter name="Kr">The reflection reflection coefficient</parameter>
   --<parameter name="Kt">The transmitted refraction coefficient</parameter>
   --<parameter name="Alpha">The shinyness power coefficient of the material</parameter>
   --<parameter name="IoR">The index of refraction of the material</parameter>
   --<exception>None at this moment</exception>

   function BDRF (Mat : in Transparent; In_Ray : in Ray; Hp : in HitPoint; Weight : in Small_Float) return RGB_Spectrum;
   --<summary>Computes the BDRF value for a material at a hitpoint for an incoming ray</summary>
   --<description>Computes the BDRF value for a material at a hitpoint for an incoming ray</description>
   --<parameter name="Mat">A material of the Material Class Wide Type</parameter>
   --<parameter name="In_Ray">The incoming ray for this hitpoint</parameter>
   --<parameter name="Hp">The hitpoint</parameter>
   --<parameter name="Weight">The weight for this ray, used to trim the recursion</parameter>
   --<exception>None at this moment</exception>


   function BDTF (Mat : in Transparent; In_Ray : in Ray; Hp : in HitPoint; Weight : in Small_Float) return RGB_Spectrum;
   --<summary>Computes the BDTF value for a material at a hitpoint for an incoming ray</summary>
   --<description>Computes the BDTF value for a material at a hitpoint for an incoming ray</description>
   --<parameter name="Mat">A material of the Material Class Wide Type</parameter>
   --<parameter name="In_Ray">The incoming ray for this hitpoint</parameter>
   --<parameter name="Hp">The hitpoint</parameter>
   --<parameter name="Weight">The weight for this ray, used to trim the recursion</parameter>
   --<exception>None at this moment</exception>

private

   type Transparent is new Material with record
      Ambient_Reflection_Spectrum   : RGB_Spectrum;
      Diffuse_Reflection_Spectrum   : RGB_Spectrum;
      Specular_Reflection_Spectrum  : RGB_Spectrum;
      Reflected_Reflection_Spectrum : RGB_Spectrum;
      Transmitted_Spectrum          : RGB_Spectrum;
      IoR                           : Small_Float;
      K_amb                         : Small_Float := 1.0;
      K_diff                        : Small_Float := 1.0;
      K_spec                        : Small_Float := 1.0;
      K_refl                        : Small_Float := 1.0;
      K_tran                        : Small_Float := 1.0;
      Alpha                         : Integer;
   end record;

end Materials.Transparent;
