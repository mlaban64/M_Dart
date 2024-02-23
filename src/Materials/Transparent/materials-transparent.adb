with Lights;                 use Lights;
with Scenes;                 use Scenes;
with Tracers;                use Tracers;
with Utilities;              use Utilities;
with Ada.Text_IO;            use Ada.Text_IO;
with Small_Float_Functions;  use Small_Float_Functions;
with Normal_Float_Functions; use Normal_Float_Functions;
with Large_Float_Functions;  use Large_Float_Functions;

package body Materials.Transparent is

   ------------------
   --  ADT Transparent
   ------------------

   function Construct_Transparent
     (Name                                                 : in String;
      Amb_Spec, Diff_Spec, Spec_Spec, Refl_Spec, Tran_Spec : in RGB_Spectrum;
      Ka, Kd, Ks, Kr, Kt                                   : in Small_Float := 1.0;
      Alpha                                                : in Integer     := 1;
      IoR                                                  : in Small_Float := 1.0) return Material_Ptr is
      Mat : Transparent_Ptr;
   begin
      Mat                               := new Transparent;
      Mat.Name                          := To_Unbounded_String (Name);
      Mat.Ambient_Reflection_Spectrum   := Amb_Spec;
      Mat.Diffuse_Reflection_Spectrum   := Diff_Spec;
      Mat.Specular_Reflection_Spectrum  := Spec_Spec;
      Mat.Reflected_Reflection_Spectrum := Refl_Spec;
      Mat.Transmitted_Spectrum          := Tran_Spec;
      Mat.K_amb                         := Ka;
      Mat.K_diff                        := Kd;
      Mat.K_spec                        := Ks;
      Mat.K_refl                        := Kr;
      Mat.K_tran                        := Kt;
      Mat.IoR                           := IoR;

      if Mat.K_amb + Mat.K_diff + Mat.K_spec + Mat.K_refl + Mat.K_tran > 1.0 then
         Debug_Message ("*** Warning: K_amb + K_diff + K_spec + K_refl + K_tran > 1.0 for Transparent " & Name, 1);
      end if;

      Mat.Alpha := Alpha;
      if Alpha < 1 then
         Debug_Message ("*** ERROR: Alpha < 1 for Transparent " & Name);
      end if;

      return Material_Ptr (Mat);
   end Construct_Transparent;

   overriding function BDRF
     (Mat    : in Transparent;
      In_Ray : in Ray;
      Hp     : in HitPoint;
      Weight : in Small_Float) return RGB_Spectrum is
      Tot_Spec, Light_Spec, Temp_Spec                                      : RGB_Spectrum;
      Lt_Ptr                                                               : Light_Ptr;
      Lt_List                                                              : Light_List;
      Surf_Normal, Light_Sample_Dir                                        : Normal_3D;
      Refl_Dir, Tran_Dir, Surf_Hp, Cam_Dir, Inv_Ray_Dir, Surf_Normal_Vec   : Vector_3D;
      No_Of_Samples                                                        : Positive;
      Max_Weight, Eta, Theta, NV_dot, Cos_Phi, Cos_Refl, Divide_By_Samples : Small_Float;
      Refl_Ray, Tran_Ray                                                   : Ray;
   begin
      Debug_Message ("=== ENTERING Transparent.BDRF with Weight " & Weight'Image, 9);
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
               Debug_Message ("*** ERROR: Cos_Phi not in [-1..1] in Transparent.BDRF in the direct illumination computation.");
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
         Debug_Message ("*** ERROR: Cos_Phi not in [0..1] in Transparent.BDRF in the reflection computation.");
         Put (Surf_Normal_Vec, "NORMAL");
         return (RED_RGB_Spec);
      end if;

      --  Compute the reflected direction
      Refl_Dir := 2.0 * Large_Float (Cos_Phi) * Surf_Normal_Vec - Inv_Ray_Dir;
      Refl_Ray := Construct_Ray (Get_WorldHp (Hp) + Ray_Epsilon * Refl_Dir, Refl_Dir);

      --  Cast the reflected ray, if the weight is high enough
      Debug_Message ("=== SPAWNING a Reflected Ray in Transparent.BDRF", 9);
      Number_Of_Reflected_Rays := Number_Of_Reflected_Rays + 1;
      Max_Weight               := Max_Weight_Spectrum (Weight, Mat.K_refl, Mat.Reflected_Reflection_Spectrum);
      Debug_Message ("MAX_WEIGHT for Reflected Ray in Transparent.BDRF = " & Max_Weight'Image, 4);
      if Max_Weight > Get_Minimum_Weight then
         Temp_Spec := Mat.K_refl * Mat.Reflected_Reflection_Spectrum * Trace_Ray (Refl_Ray, Max_Weight);
         Tot_Spec  := Tot_Spec + Temp_Spec;
      else
         Debug_Message ("*** WARNING: Reflected ray not traced in Transparent.BRDF due to weight: " & Max_Weight'Image, 3);
      end if;

      --  Adding the transmitted light. Note that the In_Ray direction needs to be inverted. Assuming only a single object in the
      --  "air"
      Eta    := 1.0 / Mat.IoR;
      NV_dot := Small_Float (Inv_Ray_Dir * Surf_Normal_Vec);
      Theta  := 1.0 - Eta * Eta * (1.0 - (NV_dot * NV_dot));

      if Theta < 0.0 then
         Debug_Message ("*** Total Internal Reflection in Transparent.BDRF. This is not implemented yet.", 1);
      --  Compute the reflected shading
      else
         --  Compute the transparent shading
         Theta    := Sqrt (Theta);
         Tran_Dir := Large_Float ((Eta * NV_dot - Theta)) * Surf_Normal_Vec - Large_Float (Eta) * Inv_Ray_Dir;

         Tran_Ray := Construct_Ray (Get_WorldHp (Hp) + Ray_Epsilon * Tran_Dir, Tran_Dir);
         --  Cast the transmitted ray, if the weight is high enough
         Debug_Message ("=== SPAWNING a Transmitted Ray in Transparent.BDRF", 9);
         Number_Of_Transmitted_Rays := Number_Of_Transmitted_Rays + 1;
         Max_Weight                 := Max_Weight_Spectrum (Weight, Mat.K_tran, Mat.Transmitted_Spectrum);
         Debug_Message ("MAX_WEIGHT for Transmitted Ray in Transparent.BDRF = " & Max_Weight'Image, 4);
         if Max_Weight > Get_Minimum_Weight then
            Temp_Spec := Mat.K_tran * Mat.Transmitted_Spectrum * Trace_Transmitted_Ray (Tran_Ray, Max_Weight);
            Tot_Spec  := Tot_Spec + Temp_Spec;
         else
            Debug_Message ("*** WARNING: Transmitted ray not traced in Transparent.BRDF due to weight: " & Max_Weight'Image, 3);
         end if;
      end if;

      Debug_Message ("=== Returned radiance in Transparent.BDRF is:", 9);
      Debug_Spectrum (Tot_Spec, 9);
      Debug_Message ("=== LEAVING Transparent.BDRF", 9);
      return Tot_Spec;
   end BDRF;

   overriding function BDTF
     (Mat    : in Transparent;
      In_Ray : in Ray;
      Hp     : in HitPoint;
      Weight : in Small_Float) return RGB_Spectrum is
      Tot_Spec, Light_Spec, Temp_Spec                                              : RGB_Spectrum;
      Lt_Ptr                                                                       : Light_Ptr;
      Lt_List                                                                      : Light_List;
      Surf_Normal, Light_Sample_Dir                                                : Normal_3D;
      Refl_Dir, Tran_Dir, Surf_Hp, Cam_Dir, Inv_Ray_Dir, Surf_Normal_Vec           : Vector_3D;
      No_Of_Samples                                                                : Positive;
      K_retr, Max_Weight, Eta, Theta, NV_dot, Cos_Phi, Cos_Refl, Divide_By_Samples : Small_Float;
      Refl_Ray, Tran_Ray                                                           : Ray;
   begin
      Debug_Message ("=== ENTERING Transparent.BDTF with Weight " & Weight'Image, 9);
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
               Debug_Message ("*** ERROR: Cos_Phi not in [-1..1] in Transparent.BDTF in the direct illumination computation.");
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
         Debug_Message ("*** ERROR: Cos_Phi not in [0..1] in Transparent.BDTF in the reflection computation.");
         Put (Surf_Normal_Vec, "NORMAL");
         return (RED_RGB_Spec);
      end if;

      --  Compute Eta/Theta to see if we have total internal reflection
      Eta    := Mat.IoR / 1.0;
      NV_dot := Small_Float (Inv_Ray_Dir * Surf_Normal_Vec);
      Theta  := 1.0 - Eta * Eta * (1.0 - (NV_dot * NV_dot));

      --  Compute the reflected direction
      Refl_Dir := 2.0 * Large_Float (Cos_Phi) * Surf_Normal_Vec - Inv_Ray_Dir;
      Refl_Ray := Construct_Ray (Get_WorldHp (Hp) + Ray_Epsilon * Refl_Dir, Refl_Dir);

      --  Adding the transmitted light. Note that the In_Ray direction needs to be inverted. Assuming only a single object in the
      --  "air"

      if Theta < 0.0 then
         Debug_Message ("=== Total Internal Reflection in Transparent.BDTF", 9);
         --  Compute the reflected shading by adding K_refl + K_tran
         Debug_Message ("=== SPAWNING an Internal Reflected Ray in Transparent.BDTF", 9);
         Number_Of_Reflected_Rays := Number_Of_Reflected_Rays + 1;
         K_retr                   := Mat.K_refl + Mat.K_tran;
         Max_Weight               := Max_Weight_Spectrum (Weight, K_retr, Mat.Reflected_Reflection_Spectrum);
         Debug_Message ("MAX_WEIGHT for Internal Reflected Ray in Transparent.BTRF = " & Max_Weight'Image, 4);
         if Max_Weight > Get_Minimum_Weight then
            Temp_Spec := K_retr * Mat.Reflected_Reflection_Spectrum * Trace_Transmitted_Ray (Refl_Ray, Max_Weight);
            Tot_Spec  := Tot_Spec + Temp_Spec;
         else
            Debug_Message
              ("*** WARNING: Internal reflected ray not traced in Transparent.BTDF due to weight: " & Max_Weight'Image,
               3);
         end if;
      else
         --  Compute the reflected shading by just using K_refl
         Debug_Message ("=== SPAWNING a Reflected Ray in Transparent.BDTF", 9);
         Number_Of_Reflected_Rays := Number_Of_Reflected_Rays + 1;
         Max_Weight               := Max_Weight_Spectrum (Weight, Mat.K_refl, Mat.Reflected_Reflection_Spectrum);
         Debug_Message ("MAX_WEIGHT for Reflected Ray in Transparent.BTRF = " & Max_Weight'Image, 4);
         if Max_Weight > Get_Minimum_Weight then
            Temp_Spec := Mat.K_refl * Mat.Reflected_Reflection_Spectrum * Trace_Transmitted_Ray (Refl_Ray, Max_Weight);
            Tot_Spec  := Tot_Spec + Temp_Spec;
         else
            Debug_Message ("*** WARNING: Reflected ray not traced in Transparent.BTDF due to weight: " & Max_Weight'Image, 3);
         end if;

         --  Compute the transparent shading
         Theta    := Sqrt (Theta);
         Tran_Dir := Large_Float ((Eta * NV_dot - Theta)) * Surf_Normal_Vec - Large_Float (Eta) * Inv_Ray_Dir;

         Tran_Ray := Construct_Ray (Get_WorldHp (Hp) + Ray_Epsilon * Tran_Dir, Tran_Dir);
         --  Cast the transmitted ray, if the weight is high enough
         Debug_Message ("=== SPAWNING a Transmitted Ray in Transparent.BDTF", 9);
         Number_Of_Transmitted_Rays := Number_Of_Transmitted_Rays + 1;
         Max_Weight                 := Max_Weight_Spectrum (Weight, Mat.K_tran, Mat.Transmitted_Spectrum);
         Debug_Message ("MAX_WEIGHT for Transmitted Ray in Transparent.BDTF = " & Max_Weight'Image, 4);
         if Max_Weight > Get_Minimum_Weight then
            Temp_Spec := Mat.K_tran * Mat.Transmitted_Spectrum * Trace_Ray (Tran_Ray, Max_Weight);
            Tot_Spec  := Tot_Spec + Temp_Spec;
         else
            Debug_Message ("*** WARNING: Transmitted ray not traced in Transparent.BTDF due to weight: " & Max_Weight'Image, 3);
         end if;
      end if;

      Debug_Message ("=== Returned radiance in Transparent.BDTF is:", 9);
      Debug_Spectrum (Tot_Spec, 9);
      Debug_Message ("=== LEAVING Transparent.BDTF", 9);
      return Tot_Spec;
   end BDTF;

end Materials.Transparent;
