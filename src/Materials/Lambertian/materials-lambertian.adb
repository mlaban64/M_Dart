with Lights;                 use Lights;
with Scenes;                 use Scenes;
with Utilities;              use Utilities;
with Ada.Text_IO;            use Ada.Text_IO;
with Small_Float_Functions;  use Small_Float_Functions;
with Normal_Float_Functions; use Normal_Float_Functions;
with Large_Float_Functions;  use Large_Float_Functions;

package body Materials.Lambertian is

   ------------------
   --  ADT Lambertian
   ------------------

   function Construct_Lambertian
     (Name                : in String;
      Amb_Spec, Diff_Spec : in RGB_Spectrum;
      Ka, Kd              : in Small_Float := 1.0) return Material_Ptr is
      Mat : Lambertian_Ptr;
   begin
      Mat                             := new Lambertian;
      Mat.Name                        := To_Unbounded_String (Name);
      Mat.Ambient_Reflection_Spectrum := Amb_Spec;
      Mat.Diffuse_Reflection_Spectrum := Diff_Spec;
      Mat.K_amb                       := Ka;
      Mat.K_diff                      := Kd;
      if Mat.K_amb + Mat.K_diff > 1.0 then
         Debug_Message ("*** Warning: K_amb + K_diff > 1.0 for Lambertian " & Name);
      end if;
      return Material_Ptr (Mat);
   end Construct_Lambertian;

   overriding function BDRF (Mat : in Lambertian; In_Ray : in Ray; Hp : in HitPoint; Weight : in Small_Float) return RGB_Spectrum is
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

      --  Compute the direct illumination contribution by looping through each light source
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
               Debug_Message ("ERROR in Lambertian.BDRF: Cos Phi out of range. Likely, one of the vectors is not normalized.");
               Put (Light_Sample_Dir);
               Put (Surf_Normal);
            end if;

            --  compute the incident radiance for this hitpoint for this sample direction to the light source. If Cos_Phi < 0, don't
            --  add luminance, because you're self-shadowing
            if Cos_Phi > 0.0 then
               --  Diffuse reflection
               Light_Spec := Lt_Ptr.Incident_Radiance (Hp, Light_Sample_Dir);
               Tot_Spec   := Tot_Spec + Divide_By_Samples * Cos_Phi * Mat.K_diff * (Light_Spec * Mat.Diffuse_Reflection_Spectrum);
            end if;

         end loop;

         --  Next light
         Lt_Ptr := Get_Next_Light (Lt_List);
      end loop;

      --  Compute the indirect illumination, which is ambient light right now
      if Use_Ambient_Light then
         Tot_Spec := Tot_Spec + Mat.K_amb * Mat.Ambient_Reflection_Spectrum * Get_Ambient_Light;
      end if;

      return Tot_Spec;
   end BDRF;

end Materials.Lambertian;
