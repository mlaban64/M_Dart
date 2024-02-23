with Tracers; use Tracers;
with Text_IO; use Text_IO;

package body Lights.Directionals is

   function Construct_Directional_Light
     (Name           : in String;
      Exitant_Energy : in Small_Float;
      Exitant_Color  : in RGB_Spectrum;
      Direction      : in Vector_3D;
      Shadow         : in Boolean) return Light_Ptr is
      New_Light : Directional_Light_Ptr;
   begin
      New_Light := new Directional_Light;
      New_Light.Set_Light_Name (To_Unbounded_String (Name));
      --  No need to set a real Sampler, because it is of no relevance for a directional light
      New_Light.Light_Sampler_Ptr := new Sampler;
      New_Light.Light_Sampler_Ptr.Initialize;

      --  Set the specific variables
      New_Light.Exitant_Energy := Exitant_Energy;
      New_Light.Exitant_Color  := Exitant_Color;
      New_Light.Direction      := Direction;
      New_Light.Dir_Normal     := -To_Normal_3D (Normalize (Direction));
      New_Light.Casts_Shadow   := Shadow;
      return Light_Ptr (New_Light);
   end Construct_Directional_Light;

   overriding procedure Put (Lt : in Directional_Light; Msg : in String := "Directional Light") is
   begin
      Put_Line ("BEGIN " & Msg);
      Put_Line ("Class = Directional_Light");
      Put ("Name = ");
      Put (Ada.Strings.Unbounded.To_String (Get_Light_Name (Lt)));
      New_Line;
      Put ("Color = ");
      Put (Lt.Exitant_Color);
      Put ("Energy = ");
      Put (Lt.Exitant_Energy);
      New_Line;
      Put ("Direction = ");
      Put (Lt.Direction);
      Put ("Dir_Normal = ");
      Put (Lt.Dir_Normal);
      Put_Line ("END " & Msg);
   end Put;

   overriding function Get_Normal_For_Next_Light_Sample
     (Lt : in Directional_Light;
      Hp : in HitPoint) return Normal_3D is
     (Lt.Dir_Normal);

   overriding function Incident_Radiance (Lt : in Directional_Light; Hp : in HitPoint; Dir : Normal_3D) return RGB_Spectrum is
      Lambda       : Large_Float := -1.0;
      New_Ray      : Ray;
      Dir_Vec      : Vector_3D;
      Inc_Radiance : RGB_Spectrum;
   begin
      --  Check if we need to cast a shadow ray for this light source
      if Lt.Casts_Shadow then

         --  Construct a ray with a little offset to avoid floating point presentation errors
         Dir_Vec := To_Vector_3D (Dir);
         New_Ray := Construct_Ray (Get_WorldHp (Hp) + Ray_Epsilon * Dir_Vec, Dir_Vec);

         --  Trace the Shadow Rays
         Lambda := Trace_Shadow_Ray (New_Ray);
         if Lambda > 0.0 then
            Inc_Radiance := BLACK_RGB_Spec;
         else
            Inc_Radiance := Lt.Exitant_Energy * Lt.Exitant_Color;
         end if;
      else
         --  No shadow ray to follow, hence compute the incident radiance directly
         Inc_Radiance := Lt.Exitant_Energy * Lt.Exitant_Color;
      end if;

      return Inc_Radiance;
   end Incident_Radiance;

end Lights.Directionals;
