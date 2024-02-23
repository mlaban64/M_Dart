with Utilities;              use Utilities;
with Ada.Text_IO;            use Ada.Text_IO;
with Small_Float_Functions;  use Small_Float_Functions;
with Normal_Float_Functions; use Normal_Float_Functions;
with Large_Float_Functions;  use Large_Float_Functions;

package body Objects.Triangles is

   ------------------
   -- ADT Triangle
   ------------------

   function Construct_Triangle (Name : in String; V1, V2, V3 : in Point_3D) return Object_Ptr is
      New_Triangle_Ptr : Triangle_Ptr;
   begin
      New_Triangle_Ptr        := new Triangle;
      New_Triangle_Ptr.Name   := To_Unbounded_String (Name);
      New_Triangle_Ptr.V1     := V1;
      New_Triangle_Ptr.V2     := V2;
      New_Triangle_Ptr.V3     := V3;
      New_Triangle_Ptr.Normal := Normalize (To_Normal_3D ((V2 - V1)**(V3 - V1)));

      --  Now calculate the intersection variables that are triangle-specific by using the version of the code on page 367 of Kevin Suffern's Ray Tracing from the Ground up
      New_Triangle_Ptr.A := Get_X (V1) - Get_X (V2);
      New_Triangle_Ptr.B := Get_X (V1) - Get_X (V3);
      New_Triangle_Ptr.E := Get_Y (V1) - Get_Y (V2);
      New_Triangle_Ptr.F := Get_Y (V1) - Get_Y (V3);
      New_Triangle_Ptr.I := Get_Z (V1) - Get_Z (V2);
      New_Triangle_Ptr.J := Get_Z (V1) - Get_Z (V3);
      return Object_Ptr (New_Triangle_Ptr);
   end Construct_Triangle;

   function Intersect (Trgl : in Triangle; R : in Ray) return Natural is
      Ray_Dir_In_OC                              : Vector_3D; -- Ray direction in object coordinates
      Ray_Org_In_OC                              : Point_3D; -- Ray origin in object coordinates
      LocalHp, WorldHp                           : Point_3D;
      LocalNv, WorldNv                           : Normal_3D;
      C, D, G, H, K, L, M, N, O, P, Q, S         : Large_Float;
      E1, E2, E3, Lambda, Inv_Denom, Beta, Gamma : Large_Float;
   begin
      -- Update the statistics
      Number_Of_Triangle_Intersections := Number_Of_Triangle_Intersections + 1;

   -- First, transform the ray into object coordinates, by applying the inverse transformation matrix of the object to the Ray.
   -- No need to normalize the direction. Note that it is normally not done to tranform a triangle, except when doing animations.
   -- However, for consistency, it is assumed possible here
      Ray_Dir_In_OC := Trgl.Trans_Inv * Get_Direction (R);
      Ray_Org_In_OC := Trgl.Trans_Inv * Get_Origin (R);

--  Now calculate the intersection by using the version of the code on page 367 of Kevin Suffern's Ray Tracing from the Ground up

      C := Get_X (Ray_Dir_In_OC);
      D := Get_X (Trgl.V1) - Get_X (Ray_Org_In_OC);
      G := Get_Y (Ray_Dir_In_OC);
      H := Get_Y (Trgl.V1) - Get_Y (Ray_Org_In_OC);
      K := Get_Z (Ray_Dir_In_OC);
      L := Get_Z (Trgl.V1) - Get_Z (Ray_Org_In_OC);

      M := Trgl.F * K - G * Trgl.J;
      N := H * K - G * L;
      P := Trgl.F * L - H * Trgl.J;
      Q := G * Trgl.I - Trgl.E * K;
      S := Trgl.E * Trgl.J - Trgl.F * Trgl.I;

      Inv_Denom := 1.0 / (Trgl.A * M + Trgl.B * Q + C * S);
      E1        := D * M - Trgl.B * N - C * P;
      Beta      := E1 * Inv_Denom;

      if Beta < 0.0 then -- no hitpoint
         return (0);
      end if;

      O     := Trgl.E * L - H * Trgl.I; -- Page 367 says r = r = e*l - h*i..., I use O
      E2    := Trgl.A * N + D * Q + C * O;
      Gamma := E2 * Inv_Denom;

      if Gamma < 0.0 then -- no hitpoint
         return (0);
      end if;

      if (Beta + Gamma) > 1.0 then -- no hitpoint
         return (0);
      end if;

      E3     := Trgl.A * P - Trgl.B * O + D * S;
      Lambda := E3 * Inv_Denom;

      if Lambda < Ray_Epsilon then -- too close/behind, so no hitpoint
         return (0);
      end if;

      -- we have a hitpoint at T, so now store it...
      LocalHp := Point_On_Ray (Ray_Org_In_OC, Ray_Dir_In_OC, Lambda);
      Push_HitPoint (Lambda, R, LocalHp, Trgl.Normal);

      return (1);
   end Intersect;

   function Hit (Trgl : in Triangle; R : in Ray) return Natural is
      Ray_Dir_In_OC                              : Vector_3D; -- Ray direction in object coordinates
      Ray_Org_In_OC                              : Point_3D; -- Ray origin in object coordinates
      LocalHp, WorldHp                           : Point_3D;
      LocalNv, WorldNv                           : Normal_3D;
      C, D, G, H, K, L, M, N, O, P, Q, S         : Large_Float;
      E1, E2, E3, Lambda, Inv_Denom, Beta, Gamma : Large_Float;
   begin
      -- Update the statistics
      Number_Of_Triangle_Intersections := Number_Of_Triangle_Intersections + 1;

   -- First, transform the ray into object coordinates, by applying the inverse transformation matrix of the object to the Ray.
   -- No need to normalize the direction. Note that it is normally not done to tranform a triangle, except when doing animations.
   -- However, for consistency, it is assumed possible here
      Ray_Dir_In_OC := Trgl.Trans_Inv * Get_Direction (R);
      Ray_Org_In_OC := Trgl.Trans_Inv * Get_Origin (R);

--  Now calculate the intersection by using the version of the code on page 367 of Kevin Suffern's Ray Tracing from the Ground up

      C := Get_X (Ray_Dir_In_OC);
      D := Get_X (Trgl.V1) - Get_X (Ray_Org_In_OC);
      G := Get_Y (Ray_Dir_In_OC);
      H := Get_Y (Trgl.V1) - Get_Y (Ray_Org_In_OC);
      K := Get_Z (Ray_Dir_In_OC);
      L := Get_Z (Trgl.V1) - Get_Z (Ray_Org_In_OC);

      M := Trgl.F * K - G * Trgl.J;
      N := H * K - G * L;
      P := Trgl.F * L - H * Trgl.J;
      Q := G * Trgl.I - Trgl.E * K;
      S := Trgl.E * Trgl.J - Trgl.F * Trgl.I;

      Inv_Denom := 1.0 / (Trgl.A * M + Trgl.B * Q + C * S);
      E1        := D * M - Trgl.B * N - C * P;
      Beta      := E1 * Inv_Denom;

      if Beta < 0.0 then -- no hitpoint
         return (0);
      end if;

      O     := Trgl.E * L - H * Trgl.I; -- Page 367 says r = r = e*l - h*i..., I use O
      E2    := Trgl.A * N + D * Q + C * O;
      Gamma := E2 * Inv_Denom;

      if Gamma < 0.0 then -- no hitpoint
         return (0);
      end if;

      if (Beta + Gamma) > 1.0 then -- no hitpoint
         return (0);
      end if;

      E3     := Trgl.A * P - Trgl.B * O + D * S;
      Lambda := E3 * Inv_Denom;

      if Lambda < Ray_Epsilon then -- too close/behind, so no hitpoint
         return (0);
      end if;

      -- we have a hitpoint at T, so now store it...
      Push_HitPoint (Lambda, R, ORIGIN_3D, X_NORM_3D);
      return (1);
   end Hit;

end Objects.Triangles;
