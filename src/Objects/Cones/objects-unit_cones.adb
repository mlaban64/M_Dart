with Utilities;              use Utilities;
with Ada.Text_IO;            use Ada.Text_IO;
with Small_Float_Functions;  use Small_Float_Functions;
with Normal_Float_Functions; use Normal_Float_Functions;
with Large_Float_Functions;  use Large_Float_Functions;

package body Objects.Unit_Cones is

   -----------------
   -- ADT Unit_Cone
   -----------------

   function Construct_Unit_Cone (Name : in String) return Object_Ptr is
      New_Cone_Ptr : Object_Ptr;
   begin
      New_Cone_Ptr      := new Unit_Cone;
      New_Cone_Ptr.Name := To_Unbounded_String (Name);
      return New_Cone_Ptr;
   end Construct_Unit_Cone;

   function Construct_Positioned_Cone (Name : in String; P1, P2 : in Point_3D; Radius : Large_Float) return Object_Ptr is
      V1, V2, V3                : Vector_3D;
      New_Cone_Ptr              : Object_Ptr;
      Len, AngleY, AngleX, CosA : Large_Float;
      Trn, Scl, RotY, RotZ      : Matrix_3D;
   begin

      if Radius < Ray_Epsilon then
         Debug_Message ("*** Warning: Radius < Epsilon in Construct_Positioned_Cone for " & Name);
      end if;

      --  Create a unit cylinder to start with
      New_Cone_Ptr      := new Unit_Cone;
      New_Cone_Ptr.Name := To_Unbounded_String (Name);

      --  Determine the translation. Remember a unit cone has its base at Y = -1, and its top at the Origin
      V1  := To_Vector_3D (P1);
      V2  := To_Vector_3D (P2);
      V3  := V2;
      Trn := Construct_Translation (V3);

      --  Determine the scaling
      Len := Length (V2 - V1);
      Scl := Construct_Scale (Radius, Len, Radius);

      --  Determine the rotation angle of To-From around the Y-axis. Then the rotation is set for around the Z-axis, although that
      --  is arbitrary. The X-axis could be chosen as well.
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

      --  Swap the angle in case the rotation around Z is < 90 degrees, or if the cone is axis-aligned with the positive Z-axis. The
      --  last is an artifact that occurs when CosA = 0. That happens when the cone is axis-aligned with Z.
      if CosA < 0.0 or Get_Z (V3) = 1.0 then
         AngleX := -AngleX;
      end if;

      RotY := Construct_Rotate_Y (AngleX);

      --  Transform the cone
      New_Cone_Ptr.Transform (Trn * RotY * RotZ * Scl);
      return New_Cone_Ptr;

   end Construct_Positioned_Cone;

   overriding function Intersect (Cone : in Unit_Cone; R : in Ray) return Natural is
      Ray_Dir_In_OC                  : Vector_3D; -- Ray direction in object coordinates
      LocalNv, WorldNv               : Normal_3D;
      Lambda1, Lambda2, Dist         : Large_Float;
      Ray_Org_In_OC                  : Point_3D; -- Ray origin in object coordinates
      LocalHp, WorldHp               : Point_3D;
      Ox, Oy, Oz, Dx, Dy, Dz, Dy_Inv : Large_Float;
      A, B, C, D, E, F               : Large_Float;
      No_Hp                          : Natural := 0;
   begin
      --  Update the statistics
      Number_Of_Unit_Cone_Intersections := Number_Of_Unit_Cone_Intersections + 1;

      --  First, transform the ray into object coordinates, by applying the inverse transformation matrix of the object to the Ray.
      --  No need to normalize the direction.
      Ray_Dir_In_OC := Cone.Trans_Inv * Get_Direction (R);
      Ray_Org_In_OC := Cone.Trans_Inv * Get_Origin (R);

      --  Determine the intersection of the ray with the Y = 0 and the Y = -1 planes
      Oy      := Get_Y (Ray_Org_In_OC);
      Dy      := Get_Y (Ray_Dir_In_OC);
      Dy_Inv  := 1.0 / Dy;
      Lambda1 := -Oy * Dy_Inv;
      Lambda2 := (-1.0 - Oy) * Dy_Inv;

      --  If both Lambda's are <= 0, we don't have any hit in front of us
      if Lambda1 < 0.0 and Lambda2 < 0.0 then
         return 0;
      end if;

      --  Check if we have an intersection with the Y = -1 circle. If so, it is always the entry or exit point, so store it.
      LocalHp := Point_On_Ray (Ray_Org_In_OC, Ray_Dir_In_OC, Lambda2);
      Ox      := Get_X (LocalHp);
      Oz      := Get_Z (LocalHp);
      --  Is the distance within a 1-radius circle? If so, add a hitpoint if in front of us
      Dist := Ox * Ox + Oz * Oz;
      if Dist <= 1.0 and Lambda2 >= 0.0 then
         LocalNv := To_Normal_3D (-Y_AXIS_3D);
         Push_HitPoint (Lambda2, R, LocalHp, LocalNv);
         No_Hp := 1;
      end if;

      --  Get the remaining ray parameters for the intersection with the infinite code. For Y they were retreived above already
      Ox := Get_X (Ray_Org_In_OC);
      Dx := Get_X (Ray_Dir_In_OC);
      Oz := Get_Z (Ray_Org_In_OC);
      Dz := Get_Z (Ray_Dir_In_OC);

      --  Now calculate the intersection by solving the quadratic formula X^2 + Z^2 = Y^2
      A := Dx * Dx + Dz * Dz - Dy * Dy;
      B := 2.0 * (Ox * Dx + Oz * Dz - Oy * Dy);
      C := Ox * Ox + Oz * Oz - Oy * Oy;

      --  Now compute the Discriminant. If => 0, we have two hitpoints. At this moment, we consider D = 0 as no hit
      D := B * B - (4.0 * A * C);
      if D > 0.0 then
         --  Precompute some factors which we have to reuse for each root
         E := 1.0 / (2.0 * A);
         F := Sqrt (D);

         --  Determine the hitpoints for each lambda. Since A and B can be negative, you can't predict the largest lambda. Hence no
         --  nested if, as with spheres and cylinders
         Lambda1 := E * (-B + F);
         Lambda2 := E * (-B - F);
         if Lambda1 > 0.0 then
            LocalHp := Point_On_Ray (Ray_Org_In_OC, Ray_Dir_In_OC, Lambda1);
            --  is the point between Y = 0 and Y = -1 ?
            Oy := Get_Y (LocalHp);
            if Oy > -1.0 and Oy < 0.0 then
               Ox      := Get_X (LocalHp);
               Oz      := Get_Z (LocalHp);
               LocalNv := To_Normal_3D (Normalize (Normalize (Construct_Vector (Ox, 0.0, Oz)) + Y_AXIS_3D));
               Push_HitPoint (Lambda1, R, LocalHp, LocalNv);
               No_Hp := No_Hp + 1;
            end if;
         end if;

         --  The second root
         Lambda2 := E * (-B - F);
         if Lambda2 > 0.0 then
            LocalHp := Point_On_Ray (Ray_Org_In_OC, Ray_Dir_In_OC, Lambda2);
            --  is the point between Y = 0 and Y = -1 ?
            Oy := Get_Y (LocalHp);
            if Oy > -1.0 and Oy < 0.0 then
               Ox      := Get_X (LocalHp);
               Oz      := Get_Z (LocalHp);
               LocalNv := To_Normal_3D (Normalize (Normalize (Construct_Vector (Ox, 0.0, Oz)) + Y_AXIS_3D));
               Push_HitPoint (Lambda2, R, LocalHp, LocalNv);
               No_Hp := No_Hp + 1;
            end if;
         end if;
      end if;
      return No_Hp;
   end Intersect;

   overriding function Hit (Cone : in Unit_Cone; R : in Ray) return Natural is
      Ray_Dir_In_OC                  : Vector_3D; -- Ray direction in object coordinates
      LocalNv, WorldNv               : Normal_3D;
      Lambda1, Lambda2, Dist         : Large_Float;
      Ray_Org_In_OC                  : Point_3D; -- Ray origin in object coordinates
      LocalHp, WorldHp               : Point_3D;
      Ox, Oy, Oz, Dx, Dy, Dz, Dy_Inv : Large_Float;
      A, B, C, D, E, F               : Large_Float;
      No_Hp                          : Natural := 0;
   begin
      --  Update the statistics
      Number_Of_Unit_Cone_Hits := Number_Of_Unit_Cone_Hits + 1;

      --  First, transform the ray into object coordinates, by applying the inverse transformation matrix of the object to the Ray.
      --  No need to normalize the direction.
      Ray_Dir_In_OC := Cone.Trans_Inv * Get_Direction (R);
      Ray_Org_In_OC := Cone.Trans_Inv * Get_Origin (R);

      --  Determine the intersection of the ray with the Y = 0 and the Y = -1 planes
      Oy      := Get_Y (Ray_Org_In_OC);
      Dy      := Get_Y (Ray_Dir_In_OC);
      Dy_Inv  := 1.0 / Dy;
      Lambda1 := -Oy * Dy_Inv;
      Lambda2 := (-1.0 - Oy) * Dy_Inv;

      --  If both Lambda's are <= 0, we don't have any hit in front of us
      if Lambda1 < 0.0 and Lambda2 < 0.0 then
         return 0;
      end if;

      --  Check if we have an intersection with the Y = -1 circle. If so, it is always the entry or exit point, so store it.
      LocalHp := Point_On_Ray (Ray_Org_In_OC, Ray_Dir_In_OC, Lambda2);
      Ox      := Get_X (LocalHp);
      Oz      := Get_Z (LocalHp);
      --  Is the distance within a 1-radius circle? If so, add a hitpoint if in front of us
      Dist := Ox * Ox + Oz * Oz;
      if Dist <= 1.0 and Lambda2 >= 0.0 then
         Push_HitPoint (Lambda2, R, ORIGIN_3D, X_NORM_3D);
         No_Hp := 1;
      end if;

      --  Get the remaining ray parameters for the intersection with the infinite code. For Y they were retreived above already
      Ox := Get_X (Ray_Org_In_OC);
      Dx := Get_X (Ray_Dir_In_OC);
      Oz := Get_Z (Ray_Org_In_OC);
      Dz := Get_Z (Ray_Dir_In_OC);

      --  Now calculate the intersection by solving the quadratic formula X^2 + Z^2 = Y^2
      A := Dx * Dx + Dz * Dz - Dy * Dy;
      B := 2.0 * (Ox * Dx + Oz * Dz - Oy * Dy);
      C := Ox * Ox + Oz * Oz - Oy * Oy;

      --  Now compute the Discriminant. If => 0, we have two hitpoints. At this moment, we consider D = 0 as no hit
      D := B * B - (4.0 * A * C);
      if D > 0.0 then
         --  Precompute some factors which we have to reuse for each root
         E := 1.0 / (2.0 * A);
         F := Sqrt (D);

         --  Determine the hitpoints for each lambda. Since A and B can be negative, you can't predict the largest lambda. Hence no
         --  nested if, as with spheres and cylinders
         Lambda1 := E * (-B + F);
         Lambda2 := E * (-B - F);
         if Lambda1 > 0.0 then
            LocalHp := Point_On_Ray (Ray_Org_In_OC, Ray_Dir_In_OC, Lambda1);
            --  is the point between Y = 0 and Y = -1 ?
            Oy := Get_Y (LocalHp);
            if Oy > -1.0 and Oy < 0.0 then
               Push_HitPoint (Lambda1, R, ORIGIN_3D, X_NORM_3D);
               No_Hp := No_Hp + 1;
            end if;
         end if;

         --  The second root
         Lambda2 := E * (-B - F);
         if Lambda2 > 0.0 then
            LocalHp := Point_On_Ray (Ray_Org_In_OC, Ray_Dir_In_OC, Lambda2);
            --  is the point between Y = 0 and Y = -1 ?
            Oy := Get_Y (LocalHp);
            if Oy > -1.0 and Oy < 0.0 then
               Push_HitPoint (Lambda2, R, ORIGIN_3D, X_NORM_3D);
               No_Hp := No_Hp + 1;
            end if;
         end if;
      end if;
      return No_Hp;
   end Hit;

end Objects.Unit_Cones;
