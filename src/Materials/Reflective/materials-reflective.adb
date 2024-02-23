with Lights;                 use Lights;
with Scenes;                 use Scenes;
with Tracers;                use Tracers;
with Utilities;              use Utilities;
with Ada.Text_IO;            use Ada.Text_IO;
with Small_Float_Functions;  use Small_Float_Functions;
with Normal_Float_Functions; use Normal_Float_Functions;
with Large_Float_Functions;  use Large_Float_Functions;

package body Materials.Reflective is

   ------------------
   --  ADT Reflective
   ------------------

   function Construct_Reflective
     (Name                                      : in String;
      Amb_Spec, Diff_Spec, Spec_Spec, Refl_Spec : in RGB_Spectrum;
      Ka, Kd, Ks, Kr                            : in Small_Float := 1.0;
      Alpha                                     : in Integer     := 1) return Material_Ptr is
      Mat : Reflective_Ptr;
   begin
      Mat                               := new Reflective;
      Mat.Name                          := To_Unbounded_String (Name);
      Mat.Ambient_Reflection_Spectrum   := Amb_Spec;
      Mat.Diffuse_Reflection_Spectrum   := Diff_Spec;
      Mat.Specular_Reflection_Spectrum  := Spec_Spec;
      Mat.Reflected_Reflection_Spectrum := Refl_Spec;
      Mat.K_amb                         := Ka;
      Mat.K_diff                        := Kd;
      Mat.K_spec                        := Ks;
      Mat.K_refl                        := Kr;

      if Mat.K_amb + Mat.K_diff + Mat.K_spec + Mat.K_refl > 1.0 then
         Debug_Message ("*** Warning: K_amb + K_diff + K_spec + K_refl > 1.0 for Reflective " & Name);
      end if;

      Mat.Alpha := Alpha;
      if Alpha < 1 then
         Debug_Message ("*** ERROR: Alpha < 1 for Reflective " & Name);
      end if;

      return Material_Ptr (Mat);
   end Construct_Reflective;

   overriding function BDRF (Mat : in Reflective; In_Ray : in Ray; Hp : in HitPoint; Weight : in Small_Float) return RGB_Spectrum is
      Tot_Spec, Light_Spec, Temp_Spec                          : RGB_Spectrum;
      Lt_Ptr                                                   : Light_Ptr;
      Lt_List                                                  : Light_List;
      Surf_Normal, Light_Sample_Dir                            : Normal_3D;
      Refl_Dir, Surf_Hp, Cam_Dir, Inv_Ray_Dir, Surf_Normal_Vec : Vector_3D;
      No_Of_Samples                                            : Positive;
      Max_Weight, Cos_Phi, Cos_Refl, Divide_By_Samples         : Small_Float;
      Refl_Ray                                                 : Ray;
   begin
      Debug_Message ("=== ENTERING Reflective.BDRF with Weight " & Weight'Image, 9);
      --  Initialize
      Lt_List         := Get_Light_List;
      Surf_Normal     := Get_WorldNv (Hp);
      Surf_Normal_Vec := To_Vector_3D (Surf_Normal);
      Surf_Hp         := To_Vector_3D (Get_WorldHp (Hp));
      Cam_Dir         := Normalize (To_Vector_3D (Get_Origin (In_Ray)) - Surf_Hp);
      Inv_Ray_Dir     := Normalize (-Get_Direction (In_Ray));

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
               Debug_Message ("*** ERROR: Cos_Phi not in [-1..1] in BDRF.Reflective in the direct illumination computation.");
               Put (Light_Sample_Dir);
               Put (Surf_Normal);
            end if;

            --  compute the incident radiance for this hitpoint for this sample direction to the light source. If Cos_Phi < 0, don't
            --  add luminance, because you're self-shadowing
            if Cos_Phi > 0.0 then
               --  Diffuse reflection
               Light_Spec := Lt_Ptr.Incident_Radiance (Hp, Light_Sample_Dir);
               Temp_Spec  := Divide_By_Samples * Cos_Phi * Mat.K_diff * (Light_Spec * Mat.Diffuse_Reflection_Spectrum);
               Tot_Spec   := Tot_Spec + Temp_Spec;

               --  Specular reflection
               Refl_Dir := Normalize (2.0 * Large_Float (Cos_Phi) * Surf_Normal_Vec - To_Vector_3D (Light_Sample_Dir));
               Cos_Refl := Small_Float (Refl_Dir * Cam_Dir);
               if Cos_Refl > 0.0 then
                  -- Adding the highlight
                  Temp_Spec :=
                    Divide_By_Samples * (Cos_Refl**Mat.Alpha) * Mat.K_spec * (Light_Spec * Mat.Specular_Reflection_Spectrum);
                  Tot_Spec := Tot_Spec + Temp_Spec;

               end if;
            end if;

         end loop;

         --  Next light
         Lt_Ptr := Get_Next_Light (Lt_List);
      end loop;

      --  Compute the indirect illumination

      --  First do the ambient light
      if Use_Ambient_Light then
         Temp_Spec := Mat.K_amb * Mat.Ambient_Reflection_Spectrum * Get_Ambient_Light;
         Tot_Spec  := Tot_Spec + Temp_Spec;
      end if;

      --  Adding the reflected light. Note that the In_Ray direction needs to be inverted
      Cos_Phi := Small_Float (Inv_Ray_Dir * Surf_Normal_Vec);

      if Cos_Phi < 0.0 or Cos_Phi > 1.0 then
         Put (Cos_Phi);
         Debug_Message ("*** ERROR: Cos_Phi not in [0..1] in BDRF.Reflective in the reflection computation.");
         Put (Surf_Normal_Vec, "NORMAL");
         return (RED_RGB_Spec);
      end if;

      --  Compute the reflected direction
      Refl_Dir := 2.0 * Large_Float (Cos_Phi) * Surf_Normal_Vec - Inv_Ray_Dir;
      Refl_Ray := Construct_Ray (Get_WorldHp (Hp) + Ray_Epsilon * Refl_Dir, Refl_Dir);

      --  Cast the relfected ray, assuming it starts in the air and has enough weight
      Number_Of_Reflected_Rays := Number_Of_Reflected_Rays + 1;
      Max_Weight               := Max_Weight_Spectrum (Weight, Mat.K_refl, Mat.Reflected_Reflection_Spectrum);
      Debug_Message ("MAX_WEIGHT in Reflective.BDRF = " & Max_Weight'Image, 4);
      if Max_Weight > Get_Minimum_Weight then
         Temp_Spec := Mat.K_refl * Mat.Reflected_Reflection_Spectrum * Trace_Ray (Refl_Ray, Max_Weight);
         Tot_Spec  := Tot_Spec + Temp_Spec;
      else
         Debug_Message ("*** WARNING: Secondary ray not traced in Reflective.BRDF due to weight: " & Max_Weight'Image, 3);
      end if;

      Debug_Message ("=== LEAVING Reflective.BDRF", 9);
      return Tot_Spec;
   end BDRF;

end Materials.Reflective;
