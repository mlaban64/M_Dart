with Lights;                 use Lights;
with Scenes;                 use Scenes;
with Utilities;              use Utilities;
with Ada.Text_IO;            use Ada.Text_IO;
with Small_Float_Functions;  use Small_Float_Functions;
with Normal_Float_Functions; use Normal_Float_Functions;
with Large_Float_Functions;  use Large_Float_Functions;

package body Materials.Phong is

   ------------------
   --  ADT Phong
   ------------------

   function Construct_Phong
     (Name                           : in String;
      Amb_Spec, Diff_Spec, Spec_Spec : in RGB_Spectrum;
      Ka, Kd, Ks                     : in Small_Float := 1.0;
      Alpha                          : in Integer     := 1) return Material_Ptr is
      Mat : Phong_Ptr;
   begin
      Mat                              := new Phong;
      Mat.Name                         := To_Unbounded_String (Name);
      Mat.Ambient_Reflection_Spectrum  := Amb_Spec;
      Mat.Diffuse_Reflection_Spectrum  := Diff_Spec;
      Mat.Specular_Reflection_Spectrum := Spec_Spec;
      Mat.K_amb                        := Ka;
      Mat.K_diff                       := Kd;
      Mat.K_spec                       := Ks;
      if Mat.K_amb + Mat.K_diff + Mat.K_spec > 1.0 then
         Debug_Message ("*** Warning: K_amb + K_diff + K_spec> 1.0 for Phong " & Name);
      end if;
      Mat.Alpha := Alpha;
      if Alpha < 1 then
         Debug_Message ("*** ERROR: Alpha < 1 for Phong " & Name);
      end if;
      return Material_Ptr (Mat);
   end Construct_Phong;

   overriding function BDRF
     (Mat    : in Phong;
      In_Ray : in Ray;
      Hp     : in HitPoint;
      Weight : in Small_Float) return RGB_Spectrum is
      Tot_Spec, Light_Spec                 : RGB_Spectrum;
      Lt_Ptr                               : Light_Ptr;
      Lt_List                              : Light_List;
      Surf_Normal                          : Normal_3D;
      Light_Sample_Dir                     : Normal_3D;
      Refl_Dir, Surf_Hp, Cam_Dir           : Vector_3D;
      No_Of_Samples                        : Positive;
      Cos_Phi, Cos_Refl, Divide_By_Samples : Small_Float;
   begin
      --  Initialize
      Lt_List     := Get_Light_List;
      Surf_Normal := Get_WorldNv (Hp);
      Surf_Hp     := To_Vector_3D (Get_WorldHp (Hp));
      Cam_Dir     := Normalize (To_Vector_3D (Get_Origin (In_Ray)) - Surf_Hp);

      --  Compute the direct illumination contribution by looping through each light
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
               Debug_Message ("ERROR in Phong.BDRF: Cos Phi out of range. Likely, one of the vectors is not normalized.");
               Put (Light_Sample_Dir);
               Put (Surf_Normal);
            end if;

            --  compute the incident radiance for this hitpoint for this sample direction to the light source. If Cos_Phi < 0, don't
            --  add luminance, because you're self-shadowing
            if Cos_Phi > 0.0 then
               --  Diffuse reflection
               Light_Spec := Lt_Ptr.Incident_Radiance (Hp, Light_Sample_Dir);
               Tot_Spec   := Tot_Spec + Divide_By_Samples * Cos_Phi * Mat.K_diff * (Light_Spec * Mat.Diffuse_Reflection_Spectrum);

               --  Specular reflection
               Refl_Dir := Normalize (2.0 * Large_Float (Cos_Phi) * To_Vector_3D (Surf_Normal) - To_Vector_3D (Light_Sample_Dir));
               Cos_Refl := Small_Float (Refl_Dir * Cam_Dir);
               if Cos_Refl > 0.0 then
                  Tot_Spec :=
                    Tot_Spec +
                    Divide_By_Samples * (Cos_Refl**Mat.Alpha) * Mat.K_spec * (Light_Spec * Mat.Specular_Reflection_Spectrum);
               end if;
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

end Materials.Phong;