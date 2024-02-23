with Utilities;              use Utilities;
with Ada.Text_IO;            use Ada.Text_IO;
with Small_Float_Functions;  use Small_Float_Functions;
with Normal_Float_Functions; use Normal_Float_Functions;
with Large_Float_Functions;  use Large_Float_Functions;

package body Objects.Unit_Cubes is

   ----------------
   -- ADT Unit_Cube
   ----------------

   function Construct_Unit_Cube (Name : in String) return Object_Ptr is
      New_Cube_Ptr : Object_Ptr;
   begin
      New_Cube_Ptr      := new Unit_Cube;
      New_Cube_Ptr.Name := To_Unbounded_String (Name);
      return New_Cube_Ptr;
   end Construct_Unit_Cube;

   overriding function Intersect (Cube : in Unit_Cube; R : in Ray) return Natural is
      Ray_Dir_In_OC                                  : Vector_3D; -- Ray direction in object coordinates
      Ray_Org_In_OC                                  : Point_3D; -- Ray origin in object coordinates
      LocalHp, WorldHp                               : Point_3D;
      LocalNv, WorldNv                               : Normal_3D;
      Tx_min, Tx_max, Ty_min, Ty_max, Tz_min, Tz_max : Large_Float;
      A, B, C, Ox, Oy, Oz, Dx, Dy, Dz, T0, T1        : Large_Float;
      Face_In, Face_Out                              : Integer;
      No_Hp                                          : Natural := 0;
   begin
      --  Update the statistics
      Number_Of_Unit_Cube_Intersections := Number_Of_Unit_Cube_Intersections + 1;

      --  First, transform the ray into object coordinates, by applying the inverse transformation matrix of the object to the Ray.
      --  No need to normalize the direction.
      Ray_Dir_In_OC := Cube.Trans_Inv * Get_Direction (R);
      Ray_Org_In_OC := Cube.Trans_Inv * Get_Origin (R);

      --  Set some variables and compute the X slab intersections
      Ox := Get_X (Ray_Org_In_OC);
      Dx := Get_X (Ray_Dir_In_OC);
      A  := 1.0 / Dx;

      if A >= 0.0 then
         Tx_min := (-1.0 - Ox) * A;
         Tx_max := (1.0 - Ox) * A;
      else
         Tx_min := (1.0 - Ox) * A;
         Tx_max := (-1.0 - Ox) * A;
      end if;

      --  Set some variables and compute the Y slab intersections
      Oy := Get_Y (Ray_Org_In_OC);
      Dy := Get_Y (Ray_Dir_In_OC);
      B  := 1.0 / Dy;

      if B >= 0.0 then
         Ty_min := (-1.0 - Oy) * B;
         Ty_max := (1.0 - Oy) * B;
      else
         Ty_min := (1.0 - Oy) * B;
         Ty_max := (-1.0 - Oy) * B;
      end if;

      --  Set some variables and compute the Z slab intersections
      Oz := Get_Z (Ray_Org_In_OC);
      Dz := Get_Z (Ray_Dir_In_OC);
      C  := 1.0 / Dz;

      if C >= 0.0 then
         Tz_min := (-1.0 - Oz) * C;
         Tz_max := (1.0 - Oz) * C;
      else
         Tz_min := (1.0 - Oz) * C;
         Tz_max := (-1.0 - Oz) * C;
      end if;

      --  Now get the largest entering value t0
      if Tx_min > Ty_min then
         T0 := Tx_min;
         if Dx < 0.0 then
            Face_In := 3; --  +x face
         else
            Face_In := 0; --  -x face
         end if;
      else
         T0 := Ty_min;
         if Dy < 0.0 then
            Face_In := 4; --  +y face
         else
            Face_In := 1; --  -y face
         end if;
      end if;
      if Tz_min > T0 then
         T0 := Tz_min;
         if Dz < 0.0 then
            Face_In := 5; --  +z face
         else
            Face_In := 2; --  -z face
         end if;
      end if;

      --  Now get the smallest exiting value t1
      if Tx_max < Ty_max then
         T1 := Tx_max;
         if Dx < 0.0 then
            Face_Out := 0; --  -x face
         else
            Face_Out := 3; --  +x face
         end if;
      else
         T1 := Ty_max;
         if Dy < 0.0 then
            Face_Out := 1; --  -y face
         else
            Face_Out := 4; --  +y face
         end if;
      end if;
      if Tz_max < T1 then
         T1 := Tz_max;
         if Dz < 0.0 then
            Face_Out := 2; --  -z face
         else
            Face_Out := 5; --  +z face
         end if;
      end if;

      --  Hit conditions: T0 before T1 and T1 in front of us
      if (T0 < T1) and (T1 > 0.0) then
         if T0 > 0.0 then
            --  both hitpoint in front of us. First compute the far hitpoint
            LocalHp := Point_On_Ray (Ray_Org_In_OC, Ray_Dir_In_OC, T1);
            LocalNv := Get_Normal_For_Cube3D (Face_Out);
            Push_HitPoint (T1, R, LocalHp, LocalNv);
            No_Hp := No_Hp + 1;

            --  then the near hitpoint
            LocalHp := Point_On_Ray (Ray_Org_In_OC, Ray_Dir_In_OC, T0);
            LocalNv := Get_Normal_For_Cube3D (Face_In);
            Push_HitPoint (T0, R, LocalHp, LocalNv);
            No_Hp := No_Hp + 1;

         else
            --  we're inside the cube. Only the far hitpoint applies. Do not inverse the normal!
            LocalHp := Point_On_Ray (Ray_Org_In_OC, Ray_Dir_In_OC, T1);
            LocalNv := Get_Normal_For_Cube3D (Face_Out);
            Push_HitPoint (T1, R, LocalHp, LocalNv);
            No_Hp := No_Hp + 1;
         end if;
      end if;
      return No_Hp;
   end Intersect;

   overriding function Hit (Cube : in Unit_Cube; R : in Ray) return Natural is
      Ray_Dir_In_OC                                  : Vector_3D; -- Ray direction in object coordinates
      Ray_Org_In_OC                                  : Point_3D; -- Ray origin in object coordinates
      LocalHp, WorldHp                               : Point_3D;
      LocalNv, WorldNv                               : Normal_3D;
      Tx_min, Tx_max, Ty_min, Ty_max, Tz_min, Tz_max : Large_Float;
      A, B, C, Ox, Oy, Oz, Dx, Dy, Dz, T0, T1        : Large_Float;
      Face_In, Face_Out                              : Integer;
      No_Hp                                          : Natural := 0;
   begin
      --  Update the statistics
      Number_Of_Unit_Cube_Hits := Number_Of_Unit_Cube_Hits + 1;

      --  First, transform the ray into object coordinates, by applying the inverse transformation matrix of the object to the Ray.
      --  No need to normalize the direction.
      Ray_Dir_In_OC := Cube.Trans_Inv * Get_Direction (R);
      Ray_Org_In_OC := Cube.Trans_Inv * Get_Origin (R);

      --  Set some variables and compute the X slab intersections
      Ox := Get_X (Ray_Org_In_OC);
      Dx := Get_X (Ray_Dir_In_OC);
      A  := 1.0 / Dx;

      if A >= 0.0 then
         Tx_min := (-1.0 - Ox) * A;
         Tx_max := (1.0 - Ox) * A;
      else
         Tx_min := (1.0 - Ox) * A;
         Tx_max := (-1.0 - Ox) * A;
      end if;

      --  Set some variables and compute the Y slab intersections
      Oy := Get_Y (Ray_Org_In_OC);
      Dy := Get_Y (Ray_Dir_In_OC);
      B  := 1.0 / Dy;

      if B >= 0.0 then
         Ty_min := (-1.0 - Oy) * B;
         Ty_max := (1.0 - Oy) * B;
      else
         Ty_min := (1.0 - Oy) * B;
         Ty_max := (-1.0 - Oy) * B;
      end if;

      --  Set some variables and compute the Z slab intersections
      Oz := Get_Z (Ray_Org_In_OC);
      Dz := Get_Z (Ray_Dir_In_OC);
      C  := 1.0 / Dz;

      if C >= 0.0 then
         Tz_min := (-1.0 - Oz) * C;
         Tz_max := (1.0 - Oz) * C;
      else
         Tz_min := (1.0 - Oz) * C;
         Tz_max := (-1.0 - Oz) * C;
      end if;

      --  Now get the largest entering value t0
      if Tx_min > Ty_min then
         T0 := Tx_min;
         if Dx < 0.0 then
            Face_In := 3; --  +x face
         else
            Face_In := 0; --  -x face
         end if;
      else
         T0 := Ty_min;
         if Dy < 0.0 then
            Face_In := 4; --  +y face
         else
            Face_In := 1; --  -y face
         end if;
      end if;
      if Tz_min > T0 then
         T0 := Tz_min;
         if Dz < 0.0 then
            Face_In := 5; --  +z face
         else
            Face_In := 2; --  -z face
         end if;
      end if;

      --  Now get the smallest exiting value t1
      if Tx_max < Ty_max then
         T1 := Tx_max;
         if Dx < 0.0 then
            Face_Out := 0; --  -x face
         else
            Face_Out := 3; --  +x face
         end if;
      else
         T1 := Ty_max;
         if Dy < 0.0 then
            Face_Out := 1; --  -y face
         else
            Face_Out := 4; --  +y face
         end if;
      end if;
      if Tz_max < T1 then
         T1 := Tz_max;
         if Dz < 0.0 then
            Face_Out := 2; --  -z face
         else
            Face_Out := 5; --  +z face
         end if;
      end if;

      --  Hit conditions: T0 before T1 and T1 in front of us
      if (T0 < T1) and (T1 > 0.0) then
         if T0 > 0.0 then
            --  both hitpoint in front of us. First compute the far hitpoint
            Push_HitPoint (T1, R, ORIGIN_3D, X_NORM_3D);
            No_Hp := No_Hp + 1;

            --  then the near hitpoint
            Push_HitPoint (T0, R, ORIGIN_3D, X_NORM_3D);
            No_Hp := No_Hp + 1;

         else
            --  we're inside the cube. Only the far hitpoint applies but the normal must be inversed
            Push_HitPoint (T1, R, ORIGIN_3D, X_NORM_3D);
            No_Hp := No_Hp + 1;
         end if;
      end if;
      return No_Hp;
   end Hit;

   --  Return the normal for a Cube3D for a face number
   function Get_Normal_For_Cube3D (Face : Integer) return Normal_3D is
   begin
      case Face is
         when 0 =>
            return Construct_Normal (-1.0, 0.0, 0.0);
         when 1 =>
            return Construct_Normal (0.0, -1.0, 0.0);
         when 2 =>
            return Construct_Normal (0.0, 0.0, -1.0);
         when 3 =>
            return Construct_Normal (1.0, 0.0, 0.0);
         when 4 =>
            return Construct_Normal (0.0, 1.0, 0.0);
         when 5 =>
            return Construct_Normal (0.0, 0.0, 1.0);
         when others =>
            Put_Line ("*** Illegal CASE value in Get_Normal_For_Cube3D");
            return Construct_Normal (1.0, 0.0, 0.0);
      end case;
   end Get_Normal_For_Cube3D;

end Objects.Unit_Cubes;
