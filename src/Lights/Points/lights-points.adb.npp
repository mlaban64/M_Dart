with Tracers; use Tracers;
with Text_IO; use Text_IO;

package body Lights.Points is

   function Construct_Point_Light
     (Name           : in String;
      Exitant_Energy : in Small_Float;
      Exitant_Color  : in RGB_Spectrum;
      Position       : in Point_3D;
      Shadow         : in Boolean) return Light_Ptr is
      New_Light : Point_Light_Ptr;
   begin
      New_Light := new Point_Light;
      New_Light.Set_Light_Name (To_Unbounded_String (Name));
      --  No need to set a real Sampler, because it is of no relevance for a directional light
      New_Light.Light_Sampler_Ptr := new Sampler;
      New_Light.Light_Sampler_Ptr.Initialize;

      --  Set the specific variables
      New_Light.Exitant_Energy := Exitant_Energy;
      New_Light.Exitant_Color  := Exitant_Color;
      New_Light.Position       := Position;
      New_Light.Casts_Shadow   := Shadow;
      return Light_Ptr (New_Light);
   end Construct_Point_Light;

   overriding procedure Put (Lt : in Point_Light; Msg : in String := "Point Light") is
   begin
      Put_Line ("BEGIN " & Msg);
      Put_Line ("Class = Point_Light");
      Put ("Name = ");
      Put (Ada.Strings.Unbounded.To_String (Get_Light_Name (Lt)));
      New_Line;
      Put ("Color = ");
      Put (Lt.Exitant_Color);
      Put ("Energy = ");
      Put (Lt.Exitant_Energy);
      New_Line;
      Put ("Position = ");
      Put (Lt.Position);
      Put_Line ("END " & Msg);
   end Put;

   overriding function Get_Normal_For_Next_Light_Sample (Lt : in Point_Light; Hp : in HitPoint) return Normal_3D is
      Dir : Normal_3D;
   begin
      Dir := Normalize (To_Normal_3D (To_Vector_3D (Lt.Position) - To_Vector_3D (Get_WorldHp (Hp))));
      return Dir;
   end Get_Normal_For_Next_Light_Sample;

   overriding function Incident_Radiance (Lt : in Point_Light; Hp : in HitPoint; Dir : Normal_3D) return RGB_Spectrum is
      Lambda       : Large_Float := -1.0;
      Lambda_Light : Large_Float;
      New_Ray      : Ray;
      Dir_Vec      : Vector_3D;
      Inc_Radiance : RGB_Spectrum;
   begin

      --  Check if we need to cast a shadow ray for this light source
      if Lt.Casts_Shadow then
         --  Construct a ray with a little offset to avoid floating point presentation errors
         Dir_Vec := To_Vector_3D (Dir);
         New_Ray := Construct_Ray (Get_WorldHp (Hp) + Ray_Epsilon * Dir_Vec, Dir_Vec);

         --  Get the Lambda for the light source for this ray, so we can determine if a hitpoint is actually between the point
         --  source and the object surface and hence casts a shadow
         Lambda_Light := Get_Lambda_For_Point (New_Ray, Lt.Position);

         --  Trace the Shadow Rays
         Lambda := Trace_Shadow_Ray (New_Ray);

         --  If Lamnda is smaller and positive, the found hitpoint is between the object and the point source
         if Lambda > 0.0 and Lambda < Lambda_Light then
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

end Lights.Points;
