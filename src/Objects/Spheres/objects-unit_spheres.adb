with Utilities;              use Utilities;
with Ada.Text_IO;            use Ada.Text_IO;
with Small_Float_Functions;  use Small_Float_Functions;
with Normal_Float_Functions; use Normal_Float_Functions;
with Large_Float_Functions;  use Large_Float_Functions;

package body Objects.Unit_Spheres is

   ------------------
   -- ADT Unit_Sphere
   ------------------

   function Construct_Unit_Sphere (Name : in String) return Object_Ptr is
      New_Sphere_Ptr : Object_Ptr;
   begin
      New_Sphere_Ptr      := new Unit_Sphere;
      New_Sphere_Ptr.Name := To_Unbounded_String (Name);
      return New_Sphere_Ptr;
   end Construct_Unit_Sphere;

   function Construct_Positioned_Sphere (Name : in String; P : in Point_3D; Radius : Large_Float) return Object_Ptr is
      New_Sphere_Ptr : Object_Ptr;
      Trn, Scl       : Matrix_3D;
   begin

      if Radius < Ray_Epsilon then
         Debug_Message ("*** Warning: Radius < Epsilon in Construct_Positioned_Sphere for " & Name);
      end if;

      --  Create a unit cylinder to start with
      New_Sphere_Ptr      := new Unit_Sphere;
      New_Sphere_Ptr.Name := To_Unbounded_String (Name);

      --  Determine the translation
      Trn := Construct_Translation (P);

      --  Determine the scaling
      Scl := Construct_Scale (Radius, Radius, Radius);

      --  Transform the cylinder
      New_Sphere_Ptr.Transform (Trn * Scl);
      return New_Sphere_Ptr;
   end Construct_Positioned_Sphere;

   overriding function Intersect (Sphere : in Unit_Sphere; R : in Ray) return Natural is
      Ray_Dir_In_OC            : Vector_3D; -- Ray direction in object coordinates
      Ray_Org_In_OC            : Point_3D; -- Ray origin in object coordinates
      LocalHp, WorldHp         : Point_3D;
      LocalNv, WorldNv         : Normal_3D;
      A, B, C, D, E, F, Lambda : Large_Float;
      No_Hp                    : Natural := 0;
   begin
      --  Update the statistics
      Number_Of_Unit_Sphere_Intersections := Number_Of_Unit_Sphere_Intersections + 1;

      --  First, transform the ray into object coordinates, by applying the inverse transformation matrix of the object to the Ray.
      --  No need to normalize the direction.
      Ray_Dir_In_OC := Sphere.Trans_Inv * Get_Direction (R);
      Ray_Org_In_OC := Sphere.Trans_Inv * Get_Origin (R);

      --  Now calculate the intersection by solving the quadratic formula Direction^2 + 2*Direction*Origin + Origin^2 = 1 This is a
      --  rewrite of X^2 + Y^2 + Z^2 = 1, where X, Y and Z are the coordinates of O + l*D
      A := Length_Squared (Ray_Dir_In_OC);
      B := 2.0 * To_Vector_3D (Ray_Org_In_OC) * Ray_Dir_In_OC;
      C := Distance_To_Org_Squared (Ray_Org_In_OC) - 1.0;

      --  Now compute the Discriminant. If > 0, we have two hitpoints. At this moment, we consider D = 0 as no hit
      D := B * B - 4.0 * A * C;
      if D > 0.0 then

         --  Precompute some factors which we have to reuse for each root
         E := 1.0 / (2.0 * A);
         F := Sqrt (D);

         --  Determine the hitpoint with largest lambda. If lambda < 0, it is behind us, and no point in determining the other
         --  root...that will be behind us as well
         Lambda := E * (-B + F);
         if Lambda > 0.0 then
            LocalHp := Point_On_Ray (Ray_Org_In_OC, Ray_Dir_In_OC, Lambda);
            LocalNv := To_Normal_3D (LocalHp);
            Push_HitPoint (Lambda, R, LocalHp, LocalNv);
            No_Hp := 1;

            --  The second root
            Lambda := E * (-B - F);
            if Lambda > 0.0 then
               LocalHp := Point_On_Ray (Ray_Org_In_OC, Ray_Dir_In_OC, Lambda);
               LocalNv := To_Normal_3D (LocalHp);
               Push_HitPoint (Lambda, R, LocalHp, LocalNv);
               No_Hp := 2;
            end if;
         end if;
      end if;
      return No_Hp;
   end Intersect;

   overriding function Hit (Sphere : in Unit_Sphere; R : in Ray) return Natural is
      Ray_Dir_In_OC            : Vector_3D; -- Ray direction in object coordinates
      Ray_Org_In_OC            : Point_3D; -- Ray origin in object coordinates
      LocalHp, WorldHp         : Point_3D;
      LocalNv, WorldNv         : Normal_3D;
      A, B, C, D, E, F, Lambda : Large_Float;
      No_Hp                    : Natural := 0;
   begin
      --  Update the statistics
      Number_Of_Unit_Sphere_Hits := Number_Of_Unit_Sphere_Hits + 1;

      --  First, transform the ray into object coordinates, by applying the inverse transformation matrix of the object to the Ray.
      --  No need to normalize the direction.
      Ray_Dir_In_OC := Sphere.Trans_Inv * Get_Direction (R);
      Ray_Org_In_OC := Sphere.Trans_Inv * Get_Origin (R);

      --  Now calculate the intersection by solving the quadratic formula Direction^2 + 2*Direction*Origin + Origin^2 = 1 This is a
      --  rewrite of X^2 + Y^2 + Z^2 = 1, where X, Y and Z are the coordinates of O + l*D
      A := Length_Squared (Ray_Dir_In_OC);
      B := 2.0 * To_Vector_3D (Ray_Org_In_OC) * Ray_Dir_In_OC;
      C := Distance_To_Org_Squared (Ray_Org_In_OC) - 1.0;

      --  Now compute the Discriminant. If > 0, we have two hitpoints. At this moment, we consider D = 0 as no hit
      D := B * B - 4.0 * A * C;
      if D > 0.0 then

         --  Precompute some factors which we have to reuse for each root
         E := 1.0 / (2.0 * A);
         F := Sqrt (D);

         --  Determine the hitpoint with largest lambda. If lambda < 0, it is behind us, and no point in determining the other
         --  root...that will be behind us as well
         Lambda := E * (-B + F);
         if Lambda > 0.0 then
            --  Push a hitpoint with an arbitrary point and normal
            Push_HitPoint (Lambda, R, ORIGIN_3D, X_NORM_3D);
            No_Hp := 1;

            --  The second root
            Lambda := E * (-B - F);
            if Lambda > 0.0 then
               --  Push a hitpoint with an arbitrary point and normal
               Push_HitPoint (Lambda, R, ORIGIN_3D, X_NORM_3D);
               No_Hp := 2;
            end if;
         end if;
      end if;
      return No_Hp;
   end Hit;

end Objects.Unit_Spheres;
