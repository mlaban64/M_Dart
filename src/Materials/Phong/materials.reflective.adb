with Lights;                 use Lights;
with Tracers;                use Tracers;
with Scenes;                 use Scenes;
with Utilities;              use Utilities;
with Small_Float_Functions;  use Small_Float_Functions;
with Normal_Float_Functions; use Normal_Float_Functions;
with Large_Float_Functions;  use Large_Float_Functions;

package body Materials.Reflective is

   ------------------
   --  ADT Reflective
   ------------------

   function Construct_Reflective
     (Name                 : in String;
      Diff_Spec, Spec_Spec :    RGB_Spectrum;
      Kd, Ks               :    Small_Float) return Material_Ptr is
      Mat : Reflective_Ptr;
   begin
      Mat                              := new Reflective;
      Mat.Name                         := To_Unbounded_String (Name);
      Mat.Diffuse_Reflection_Spectrum  := Diff_Spec;
      Mat.Specular_Reflection_Spectrum := Spec_Spec;
      Mat.Kd                           := Kd;
      Mat.Ks                           := Ks;
      if (Kd + Ks) /= 1.0 then
         Debug_Message ("*** Warning: Kd + Ks <> 1.0");
      end if;
      return Material_Ptr (Mat);
   end Construct_Reflective;

   overriding function BDRF (Mat : in Reflective; In_Ray : in Ray; Hp : in HitPoint) return RGB_Spectrum is
      Tot_Spec, Light_Spec : RGB_Spectrum;
      Lt_Ptr               : Light_Ptr;
      Lt_List              : Light_List;
      Surf_Normal          : Normal_3D;
      Light_Sample_Dir     : Normal_3D;
      Cos_Phi              : Small_Float;
      No_Of_Samples        : Positive;
      Divide_By_Samples    : Small_Float;
   begin
      --  Initialize
      Lt_List     := Get_Light_List;
      Surf_Normal := Get_WorldNv (Hp);

      --  Compute the direct illumination contribution Loop through each light
      Lt_Ptr := Get_First_Light (Lt_List);
      while Lt_Ptr /= null loop

         --  Since the interaction between a light source and a material is highly depending on the material, and less of the light
         --  source, multi-sampling of the light source is done via the material, so overhere
         No_Of_Samples     := Lt_Ptr.Get_Light_Sampler.Get_No_Of_Samples;
         Divide_By_Samples := 1.0 / Small_Float (No_Of_Samples);

         for i in 1 .. No_Of_Samples loop

            --  Lambertian computation: Compute the angle between the surface normal and the normal pointing to the light
            Light_Sample_Dir := Lt_Ptr.Get_Normal_For_Next_Light_Sample (Hp);
            Cos_Phi          := Small_Float (Light_Sample_Dir * Surf_Normal);

            --  Checking Cos_Phi for debugging purposes. It could be removed after ample testing has passed
            if Cos_Phi > 1.0 or Cos_Phi < -1.0 then
               Debug_Message ("ERROR in Reflective.BDRF: Cos Phi out of range. Likely, one of the vectors is not normalized.");
               Put (Light_Sample_Dir);
               Put (Surf_Normal);
            end if;

            --  compute the incident radiance for this hitpoint for this sample direction to the light source. If Cos_Phi < 0, don't
            --  add luminance, because you're self-shadowing. Multiply by the diffuse reflection coefficient
            if Cos_Phi > 0.0 then
               Light_Spec := Lt_Ptr.Incident_Radiance (Hp, Light_Sample_Dir) * Cos_Phi;
               Tot_Spec   := Tot_Spec + Mat.Kd * Divide_By_Samples * (Light_Spec * Mat.Diffuse_Reflection_Spectrum);
            end if;

            --  ADD THE PHONG COMPONENT FOR THE LIGHT SOURCE REFLECTION
         end loop;

         --  Next light
         Lt_Ptr := Get_Next_Light (Lt_List);
      end loop;

      --  ADD THE REFLECTED COMPONENT

      return Tot_Spec;
   end BDRF;

end Materials.Reflective;
