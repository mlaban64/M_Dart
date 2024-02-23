with Utilities;              use Utilities;
with Ada.Text_IO;            use Ada.Text_IO;
with Small_Float_Functions;  use Small_Float_Functions;
with Normal_Float_Functions; use Normal_Float_Functions;
with Large_Float_Functions;  use Large_Float_Functions;

package body Objects.Unit_Cylinders is

   ------------------
   -- ADT Unit_Cylinder
   ------------------

   function Construct_Unit_Cylinder (Name : in String) return Object_Ptr is
      New_Cylinder_Ptr : Object_Ptr;
   begin
      New_Cylinder_Ptr      := new Unit_Cylinder;
      New_Cylinder_Ptr.Name := To_Unbounded_String (Name);
      return New_Cylinder_Ptr;
   end Construct_Unit_Cylinder;

   function Construct_Positioned_Cylinder (Name : in String; P1, P2 : in Point_3D; Radius : Large_Float) return Object_Ptr is
      V1, V2, V3                : Vector_3D;
      New_Cylinder_Ptr          : Object_Ptr;
      Len, AngleY, AngleX, CosA : Large_Float;
      Trn, Scl, RotY, RotZ      : Matrix_3D;
   begin

      if Radius < Ray_Epsilon then
         Debug_Message ("*** Warning: Radius < Epsilon in Construct_Positioned_Cylinder for " & Name);
      end if;

      --  Create a unit cylinder to start with
      New_Cylinder_Ptr      := new Unit_Cylinder;
      New_Cylinder_Ptr.Name := To_Unbounded_String (Name);

      --  Determine the translation
      V1  := To_Vector_3D (P1);
      V2  := To_Vector_3D (P2);
      V3  := V1 + 0.5 * (V2 - V1);
      Trn := Construct_Translation (V3);

      --  Determine the scaling
      Len := Length (V2 - V1) / 2.0;
      Scl := Construct_Scale (Radius, Len, Radius);

      --  Determine the rotation angle with the Y-axis
      V3     := Normalize (V2 - V1);
      CosA   := V3 * Y_AXIS_3D;
      AngleY := Arccos (CosA);
      RotZ   := Construct_Rotate_Z (-AngleY);

      --  angle with the X-axis
      V3 := Construct_Vector (Get_X (V3), 0.0, Get_Z (V3));
      if Length (V3) > 0.0 then
         V3     := Normalize (V3);
         AngleX := Arccos (V3 * X_AXIS_3D);
      else
         AngleX := 0.0;
      end if;

      --  Swap the angle in case the rotation around Z is < 90 degrees
      if CosA < 0.0 then
         AngleX := -AngleX;
      end if;
      RotY := Construct_Rotate_Y (AngleX);

      --  Transform the cylinder
      New_Cylinder_Ptr.Transform (Trn * RotY * RotZ * Scl);
      return New_Cylinder_Ptr;
   end Construct_Positioned_Cylinder;

   overriding function Intersect (Cylinder : in Unit_Cylinder; R : in Ray) return Natural is
      Ray_Dir_In_OC                  : Vector_3D; -- Ray direction in object coordinates
      Ray_Org_In_OC                  : Point_3D; -- Ray origin in object coordinates
      LocalHp, WorldHp               : Point_3D;
      LocalNv, WorldNv               : Normal_3D;
      Lambda1, Lambda2, Dist         : Large_Float;
      Ox, Oy, Oz, Dx, Dy, Dz, Dy_Inv : Large_Float;
      A, B, C, D, E, F               : Large_Float;
      No_Hp                          : Natural := 0;
   begin
      --  Update the statistics
      Number_Of_Unit_Cylinder_Intersections := Number_Of_Unit_Cylinder_Intersections + 1;

      --  First, transform the ray into object coordinates, by applying the inverse transformation matrix of the object to the Ray.
      --  No need to normalize the direction.
      Ray_Dir_In_OC := Cylinder.Trans_Inv * Get_Direction (R);
      Ray_Org_In_OC := Cylinder.Trans_Inv * Get_Origin (R);

      --  Determine the intersection of the ray with the Y = 1 and the Y = -1 plane
      Oy      := Get_Y (Ray_Org_In_OC);
      Dy      := Get_Y (Ray_Dir_In_OC);
      Dy_Inv  := 1.0 / Dy;
      Lambda1 := (1.0 - Oy) * Dy_Inv;
      Lambda2 := (-1.0 - Oy) * Dy_Inv;

      --  If both Lambda's are <= 0, we don't have any hit in front of us
      if Lambda1 <= 0.0 and Lambda2 <= 0.0 then
         return 0;
      end if;

      --  Check if we have an intersection with the Y = 1 circle. If so, it is always the entry or exit point, so store it.
      LocalHp := Point_On_Ray (Ray_Org_In_OC, Ray_Dir_In_OC, Lambda1);
      Ox      := Get_X (LocalHp);
      Oz      := Get_Z (LocalHp);
      --  Is the distance within a 1-radius circle? If so, add a hitpoint if in front of us
      Dist := Ox * Ox + Oz * Oz;
      if Dist < 1.0 and Lambda1 > 0.0 then
         LocalNv := To_Normal_3D (Y_AXIS_3D);
         Push_HitPoint (Lambda1, R, LocalHp, LocalNv);
         No_Hp := 1;
      end if;

      --  Check if we have an intersection with the Y = -1 circle. If so, it is always the entry or exit point, so store it.
      LocalHp := Point_On_Ray (Ray_Org_In_OC, Ray_Dir_In_OC, Lambda2);
      Ox      := Get_X (LocalHp);
      Oz      := Get_Z (LocalHp);
      --  Is the distance within a 1-radius circle? If so, add a hitpoint if in front of us
      Dist := Ox * Ox + Oz * Oz;
      if Dist < 1.0 and Lambda2 > 0.0 then
         LocalNv := To_Normal_3D (-Y_AXIS_3D);
         Push_HitPoint (Lambda2, R, LocalHp, LocalNv);
         No_Hp := No_Hp + 1;
      end if;

      --  If both circles are hit, no point to continue as no other hitpoints can be valid
      if No_Hp = 2 then
         return No_Hp;
      end if;

      --  No two hitpoints found, so we now need to check the general cylinder First, calculate the factors of the quadratic formula
      Dx := Get_X (Ray_Dir_In_OC);
      Dz := Get_Z (Ray_Dir_In_OC);
      Ox := Get_X (Ray_Org_In_OC);
      Oz := Get_Z (Ray_Org_In_OC);
      A  := Dx * Dx + Dz * Dz;
      B  := 2.0 * (Ox * Dx + Oz * Dz);
      C  := Ox * Ox + Oz * Oz - 1.0;
      --  Calculate the discriminant. If D > 0, then we have two hits
      D := B * B - 4.0 * A * C;
      if D > 0.0 then
         --  Precompute some factors which we have to reuse for each root
         E := 1.0 / (2.0 * A);
         F := Sqrt (D);
         --  Determine the hitpoint with largest lambda. If lambda < 0, it is behind us, and no point in determining the other
         --  root...that will be behind us as well
         Lambda1 := E * (-B + F);
         if Lambda1 > 0.0 then
            LocalHp := Point_On_Ray (Ray_Org_In_OC, Ray_Dir_In_OC, Lambda1);
            --  is the point between Y = 1 and Y = -1 ?
            Oy := Get_Y (LocalHp);
            if Oy > -1.0 and Oy < 1.0 then
               Ox      := Get_X (LocalHp);
               Oz      := Get_Z (LocalHp);
               LocalNv := Construct_Normal (Ox, 0.0, Oz);
               Push_HitPoint (Lambda1, R, LocalHp, LocalNv);
               No_Hp := No_Hp + 1;
            end if;

            --  The second root
            Lambda1 := E * (-B - F);
            if Lambda1 > 0.0 then
               LocalHp := Point_On_Ray (Ray_Org_In_OC, Ray_Dir_In_OC, Lambda1);
               --  is the point between Y = 1 and Y = -1 ?
               Oy := Get_Y (LocalHp);
               if Oy > -1.0 and Oy < 1.0 then
                  Ox      := Get_X (LocalHp);
                  Oz      := Get_Z (LocalHp);
                  LocalNv := Construct_Normal (Ox, 0.0, Oz);
                  Push_HitPoint (Lambda1, R, LocalHp, LocalNv);
                  No_Hp := No_Hp + 1;
               end if;
            end if;
         end if;
      end if;
      return No_Hp;
   end Intersect;

   overriding function Hit (Cylinder : in Unit_Cylinder; R : in Ray) return Natural is
      Ray_Dir_In_OC                  : Vector_3D; -- Ray direction in object coordinates
      Ray_Org_In_OC                  : Point_3D; -- Ray origin in object coordinates
      LocalHp, WorldHp               : Point_3D;
      LocalNv, WorldNv               : Normal_3D;
      Lambda1, Lambda2, Dist         : Large_Float;
      Ox, Oy, Oz, Dx, Dy, Dz, Dy_Inv : Large_Float;
      A, B, C, D, E, F               : Large_Float;
      No_Hp                          : Natural := 0;
   begin
      --  Update the statistics
      Number_Of_Unit_Cylinder_Hits := Number_Of_Unit_Cylinder_Hits + 1;

      --  First, transform the ray into object coordinates, by applying the inverse transformation matrix of the object to the Ray.
      --  No need to normalize the direction.
      Ray_Dir_In_OC := Cylinder.Trans_Inv * Get_Direction (R);
      Ray_Org_In_OC := Cylinder.Trans_Inv * Get_Origin (R);

      --  Determine the intersection of the ray with the Y = 1 and the Y = -1 plane
      Oy      := Get_Y (Ray_Org_In_OC);
      Dy      := Get_Y (Ray_Dir_In_OC);
      Dy_Inv  := 1.0 / Dy;
      Lambda1 := (1.0 - Oy) * Dy_Inv;
      Lambda2 := (-1.0 - Oy) * Dy_Inv;

      --  If both Lambda's are <= 0, we don't have any hit in front of us
      if Lambda1 <= 0.0 and Lambda2 <= 0.0 then
         return 0;
      end if;

      --  Check if we have an intersection with the Y = 1 circle. If so, it is always the entry or exit point, so store it.
      LocalHp := Point_On_Ray (Ray_Org_In_OC, Ray_Dir_In_OC, Lambda1);
      Ox      := Get_X (LocalHp);
      Oz      := Get_Z (LocalHp);
      --  Is the distance within a 1-radius circle? If so, add a hitpoint if in front of us
      Dist := Ox * Ox + Oz * Oz;
      if Dist < 1.0 and Lambda1 > 0.0 then
         Push_HitPoint (Lambda1, R, ORIGIN_3D, X_NORM_3D);
         No_Hp := 1;
      end if;

      --  Check if we have an intersection with the Y = -1 circle. If so, it is always the entry or exit point, so store it.
      LocalHp := Point_On_Ray (Ray_Org_In_OC, Ray_Dir_In_OC, Lambda2);
      Ox      := Get_X (LocalHp);
      Oz      := Get_Z (LocalHp);
      --  Is the distance within a 1-radius circle? If so, add a hitpoint if in front of us
      Dist := Ox * Ox + Oz * Oz;
      if Dist < 1.0 and Lambda2 > 0.0 then
         Push_HitPoint (Lambda2, R, ORIGIN_3D, X_NORM_3D);
         No_Hp := No_Hp + 1;
      end if;

      --  If both circles are hit, no point to continue as no other hitpoints can be valid
      if No_Hp = 2 then
         return No_Hp;
      end if;

      --  No two hitpoints found, so we now need to check the general cylinder First, calculate the factors of the quadratic formula
      Dx := Get_X (Ray_Dir_In_OC);
      Dz := Get_Z (Ray_Dir_In_OC);
      Ox := Get_X (Ray_Org_In_OC);
      Oz := Get_Z (Ray_Org_In_OC);
      A  := Dx * Dx + Dz * Dz;
      B  := 2.0 * (Ox * Dx + Oz * Dz);
      C  := Ox * Ox + Oz * Oz - 1.0;
      --  Calculate the discriminant. If D > 0, then we have two hits
      D := B * B - 4.0 * A * C;
      if D > 0.0 then
         --  Precompute some factors which we have to reuse for each root
         E := 1.0 / (2.0 * A);
         F := Sqrt (D);
         --  Determine the hitpoint with largest lambda. If lambda < 0, it is behind us, and no point in determining the other
         --  root...that will be behind us as well
         Lambda1 := E * (-B + F);
         if Lambda1 > 0.0 then
            LocalHp := Point_On_Ray (Ray_Org_In_OC, Ray_Dir_In_OC, Lambda1);
            --  is the point between Y = 1 and Y = -1 ?
            Oy := Get_Y (LocalHp);
            if Oy > -1.0 and Oy < 1.0 then
               Push_HitPoint (Lambda1, R, ORIGIN_3D, X_NORM_3D);
               No_Hp := No_Hp + 1;
            end if;

            --  The second root
            Lambda1 := E * (-B - F);
            if Lambda1 > 0.0 then
               LocalHp := Point_On_Ray (Ray_Org_In_OC, Ray_Dir_In_OC, Lambda1);
               --  is the point between Y = 1 and Y = -1 ?
               Oy := Get_Y (LocalHp);
               if Oy > -1.0 and Oy < 1.0 then
                  Push_HitPoint (Lambda1, R, ORIGIN_3D, X_NORM_3D);
                  No_Hp := No_Hp + 1;
               end if;
            end if;
         end if;
      end if;
      return No_Hp;
   end Hit;

end Objects.Unit_Cylinders;
